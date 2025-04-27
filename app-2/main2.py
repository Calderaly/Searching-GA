import random, math, time

# Konfigurasi populasi
BITS_PER_VARIABEL = 32  # Jumlah bit per variabel
RANGE_UPPER_BOUND = 10.0  # Batas atas x1 dan x2
RANGE_LOWER_BOUND = -10.0  # Batas bawah x1 dan x2
RANGE_DOMAIN = RANGE_UPPER_BOUND - RANGE_LOWER_BOUND  # Rentang x1 dan x2
CHROMOSOME_LENGTH = BITS_PER_VARIABEL * 2  # Panjang kromosom

# Parameter GA
POPULATION_SIZE = 2000  # Ukuran populasi
GENERATIONS_SIZE = 200  # Jumlah generasi
BEST_INDIVIDUAL = 2  # Jumlah individu terbaik yang dipertahankan
GENERATIONS_WITHOUT_IMPROVEMENT = 40  # Batas untuk hasil yang stagnan

# Operator GA
CROSSOVER_RATE = 0.8  # Peluang crossover
MUTATION_RATE = 0.01  # Peluang mutasi
ROULLETE_SIZE = 5  # Jumlah seleksi pada rolet untuk mendapatkan parent


def InitPopulation(populationSize):
    """
    Inisialisasi populasi awal secara acak.

    Args:
        populationSize: Ukuran populasi.

    Returns:
        List: Populasi yang berisi kromosom-kromosom biner.
    """
    population = []
    for _ in range(populationSize):
        population.append(''.join(random.choice('01') for _ in range(CHROMOSOME_LENGTH)))
    return population


def ObjectiveFunction(x1, x2):
    """
    Fungsi objektif yang akan dioptimasi (minimisasi).

    Args:
        x1: Nilai variabel x1.
        x2: Nilai variabel x2.

    Returns:
        float: Hasil perhitungan fungsi objektif.
    """
    try:
        term1 = math.sin(x1) * math.cos(x2) * math.tan(x1 + x2)
        term2 = (3 / 4) * math.exp(1 - math.sqrt(x1 ** 2))
        result = -(term1 + term2)  # Minimalkan fungsi ini

        # Mengecek hasil dari perhitungan dalam if-else untuk menyederhanakan error handling
        if math.isnan(result) or math.isinf(result):
            return float('-inf')
        else:
            return result
    except OverflowError:
        return float('-inf')



def BinaryToFloat(binaryStr):
    """
    Mengonversi string biner menjadi nilai float dalam rentang yang ditentukan.

    Args:
        binaryStr: String biner yang akan dikonversi.

    Returns:
        float: Nilai float yang dihasilkan.
    """
    decimal = int(binaryStr, 2)
    return RANGE_LOWER_BOUND + (decimal / ((1 << BITS_PER_VARIABEL) - 1)) * RANGE_DOMAIN


def DecodeChromosome(chromosome):
    """
    Mendekode kromosom menjadi nilai x1 dan x2.

    Args:
        chromosome: String biner yang merepresentasikan kromosom.

    Returns:
        tuple: Tuple yang berisi nilai x1 dan x2 sebagai float.
    """
    binX1 = chromosome[:BITS_PER_VARIABEL]
    binx2 = chromosome[BITS_PER_VARIABEL:]
    floatX1 = BinaryToFloat(binX1)
    floatX2 = BinaryToFloat(binx2)
    return floatX1, floatX2



def CalculateFitnessValues(population):
    """
    Menghitung nilai fitness untuk setiap kromosom dalam populasi.

    Args:
        population: List yang berisi kromosom-kromosom biner.

    Returns:
        List: List yang berisi dictionary dengan informasi kromosom, x1, x2, dan nilai fitness.
    """
    fitness_values = []
    for chromosome in population:
        x1, x2 = DecodeChromosome(chromosome)
        fitness = ObjectiveFunction(x1, x2)
        fitness_values.append({
            'chromosome': chromosome,
            'x1': x1,
            'x2': x2,
            'objective': fitness
        })
    return fitness_values



def RoulletteWheelSelection(population, fitnessValues, size):
    """
    Melakukan seleksi menggunakan metode rolet.

    Args:
        population: List kromosom.
        fitnessValues: List nilai fitness yang sesuai dengan populasi.
        size: Jumlah parent yang akan dipilih.

    Returns:
        List: List kromosom yang terpilih sebagai parent.
    """
    objectiveValues = [item['objective'] for item in fitnessValues]
    maxObjective = max(objectiveValues)
    adjustedFitnessValue = [maxObjective - obj for obj in objectiveValues]

    if all(f == fitnessValues[0] for f in fitnessValues):
        return random.choices(population, k=size)

    totalAdjustedFitness = sum(adjustedFitnessValue)
    winner = [(f / totalAdjustedFitness) for f in adjustedFitnessValue]
    parents = random.choices(population, winner, k=size)
    return parents



def UniformCrossover(parent1, parent2, crossoverRate):
    """
    Melakukan crossover uniform pada dua parent.

    Args:
        parent1: Kromosom parent pertama.
        parent2: Kromosom parent kedua.
        crossoverRate: Peluang terjadinya crossover.

    Returns:
        tuple: Tuple yang berisi kromosom child1 dan child2 hasil crossover.
    """
    child1 = ''
    child2 = ''
    for i in range(len(parent1)):
        if random.random() < crossoverRate:
            child1 += parent1[i]
            child2 += parent2[i]
        else:
            child1 += parent2[i]
            child2 += parent1[i]
    return child1, child2



def ScrambleMutation(chromosome):
    """
    Melakukan mutasi scramble pada kromosom.

    Args:
        chromosome: Kromosom yang akan dimutasi.

    Returns:
        str: Kromosom hasil mutasi.
    """
    if random.random() < MUTATION_RATE:
        length = CHROMOSOME_LENGTH
        i, j = sorted(random.sample(range(length), 2))
        segment = list(chromosome[i:j + 1])
        random.shuffle(segment)
        return chromosome[:i] + ''.join(segment) + chromosome[j + 1:]
    return chromosome



def main():
    """
    Fungsi utama yang menjalankan algoritma genetika.
    """
    startTime = time.time()
    population = InitPopulation(POPULATION_SIZE)
    bestSolutionOverall = None
    lastBestFitness = float('-inf')
    noImprovementCount = 0
    parent1_chromosome_best = ""
    parent2_chromosome_best = ""

    for generasi in range(GENERATIONS_SIZE):
        evaluatedPopulation = CalculateFitnessValues(population)
        # Sort ascending untuk minimalisasi 'reverse = False', sort descending untuk maksimalisasi 'reverse = True'
        evaluatedPopulation.sort(key=lambda x: x['objective'], reverse=False)

        bestIndividual = evaluatedPopulation[0]  # Dapatkan individu terbaik
        parents = RoulletteWheelSelection(population, evaluatedPopulation, ROULLETE_SIZE)
        parent1_chromosome = parents[0]
        parent2_chromosome = parents[1]
        if bestSolutionOverall is None or bestIndividual['objective'] > bestSolutionOverall['objective']:
            bestSolutionOverall = bestIndividual
            lastBestFitness = bestIndividual['objective']
            noImprovementCount = 0
            parent1_chromosome_best = parent1_chromosome
            parent2_chromosome_best = parent2_chromosome
        else:
            noImprovementCount += 1
        
        x1_bin = bestIndividual['chromosome'][:BITS_PER_VARIABEL]
        x2_bin = bestIndividual['chromosome'][BITS_PER_VARIABEL:]
        fitness_bin =  ''.join(['1' if bit == '1' else '0' for bit in bin(math.ceil(bestIndividual['objective']))[2:].zfill(
            BITS_PER_VARIABEL)])
        print(f"generasi {generasi + 1}/{GENERATIONS_SIZE} = x1: {bestIndividual['x1']:.4f} (bin: {x1_bin}), x2: 
        {bestIndividual['x2']:.4f} (bin: {x2_bin}), fitness: {bestIndividual['objective']:.4f} (bin: {fitness_bin})")
        if generasi > 0:
            print(f"  Orang Tua Terbaik: {parent1_chromosome_best[:10]}...{parent1_chromosome_best[-10:]}, 
            {parent2_chromosome_best[:10]}...{parent2_chromosome_best[-10:]}")

        newPopulation = [evaluatedPopulation[i]['chromosome'] for i in range(BEST_INDIVIDUAL)]  # Elitism
        
        while len(newPopulation) < POPULATION_SIZE:
            child1, child2 = UniformCrossover(parent1_chromosome, parent2_chromosome, CROSSOVER_RATE)
            child1 = ScrambleMutation(child1)
            child2 = ScrambleMutation(child2)
            newPopulation.extend([child1, child2])
            if len(newPopulation) > POPULATION_SIZE:
                newPopulation.pop()
        
        population = newPopulation

    endTime = time.time()
    # Hasil akhir
    print("-" * 30)
    print("---Evolusi Selesai---")
    print(f"Waktu eksekusi: {endTime - startTime:.2f} detik")

    if bestSolutionOverall:
        print("\nSolusi terbaik ditemukan (Minimalisasi): ")
        print(f"Kromosom: {bestSolutionOverall['chromosome'][:15]}...{bestSolutionOverall['chromosome'][-15:]}")
        print(f"Nilai x1 dan x2: ({bestSolutionOverall['x1']:.4f}, {bestSolutionOverall['x2']:.4f})")
        print(f"Nilai Fungsi Objektif (Minimum): {bestSolutionOverall['objective']:.6f}")
    else:
        print("\nTidak ada solusi terbaik yang ditemukan.")

    # Ringkasan dari nilai loop fitness, rekombinasi, dan mutasi.
    print("\nTop 3 individu di populasi terakhir:")
    finalEvaluatedPopulation = CalculateFitnessValues(population)
    finalEvaluatedPopulation.sort(key=lambda x: x['objective'], reverse=False)
    for i, ind in enumerate(finalEvaluatedPopulation[:3]):
        x1_bin_top3 = ind['chromosome'][:BITS_PER_VARIABEL]
        x2_bin_top3 = ind['chromosome'][BITS_PER_VARIABEL:]
        fitness_bin_top3 = ''.join(['1' if bit == '1' else '0' for bit in bin(math.ceil(ind['objective']))[2:].zfill(
            BITS_PER_VARIABEL)])
        print(f"  {i + 1}. Nilai fitness: {ind['objective']:.6f} (bin: {fitness_bin_top3})\n (x1 = {ind['x1']:.4f} (bin: 
        {x1_bin_top3})\n x2 = {ind['x2']:.4f} (bin: {x2_bin_top3}))")



if __name__ == "__main__":
    main()