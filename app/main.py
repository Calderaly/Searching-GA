import random, math, time

# Konfigurasi populasi
BITS_PER_VARIABEL = 32   # Jumlah bit per variabel
RANGE_UPPER_BOUND = 10.0   # Batas atas x1 dan x2
RANGE_LOWER_BOUND = -10.0  # Batas bawah x1 dan x2
RANGE_DOMAIN = RANGE_UPPER_BOUND - RANGE_LOWER_BOUND   # Rentang x1 dan x2
CHROMOSOME_LENGTH = BITS_PER_VARIABEL * 2   # Panjang kromosom

# Parameter GA
POPULATION_SIZE = 2000   # Ukuran populasi
GENERATIONS_SIZE = 200   # Jumlah generasi
BEST_INDIVIDUAL = 2   # Jumlah individu terbaik yang dipertahankan
GENERATIONS_WITHOUT_IMPROVEMENT = 20   # Batas untuk hasil yang stagnan

# Operator GA
CROSSOVER_RATE = 0.8   # Peluang crossover
MUTATION_RATE = 0.01  # Peluang mutasi
ROULLETE_SIZE = 5   # Jumlah seleksi pada rolet untuk mendapatkan parent
SIGNIFICANT_CHANGE_THRESHOLD = 0.001   # Ambang batas perubahan signifikan


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
        result = -(term1 + term2)   # Minimalkan fungsi ini

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


def geneticAlgorithm(population):
    bestSolutionOverall = None
    lastBestFitness = float('-inf')
    noImprovementCount = 0
    generasiStagnant = 0  # Penghitung generasi stagnan
    global MUTATION_RATE, POPULATION_SIZE # Memberi tahu Python bahwa variabel global ini akan diakses oleh variabel lokal 

    # Track nilai-nilai terbaik sebelumnya untuk membandingkan
    previousBestValues = []

    for generasi in range(GENERATIONS_SIZE):
        evaluatedPopulation = CalculateFitnessValues(population)
        evaluatedPopulation.sort(key=lambda x: x['objective'], reverse=False)  # Urutkan menaik untuk minimisasi

        bestIndividual = evaluatedPopulation[0]  # Dapatkan individu terbaik

        # Simpan nilai terbaik untuk pengecekan stagnasi
        previousBestValues.append(bestIndividual['objective'])
        if len(previousBestValues) > 5:  # Simpan 5 generasi terakhir
            previousBestValues.pop(0)

        # Cek apakah nilai objektif stagnan dalam 5 generasi terakhir
        isStagnant = len(previousBestValues) >= 3 and len(set([round(x, 6) for x in previousBestValues])) == 1

        if bestSolutionOverall is None or bestIndividual['objective'] > bestSolutionOverall['objective']:
            bestSolutionOverall = bestIndividual
            fitness_change = abs(bestIndividual['objective'] - lastBestFitness)
            if fitness_change > SIGNIFICANT_CHANGE_THRESHOLD:
                lastBestFitness = bestIndividual['objective']
                noImprovementCount = 0
                generasiStagnant = 0  # Reset penghitung stagnasi
            else:
                noImprovementCount += 1
                generasiStagnant += 1
        else:
            noImprovementCount += 1
            generasiStagnant += 1

        # Cetak hasil generasi saat ini
        print(f"generasi {generasi + 1}/{GENERATIONS_SIZE} = x1: {bestIndividual['x1']:.4f}, x2: {bestIndividual['x2']:.4f}, fitness: {bestIndividual['objective']:.6f}")

        # Tambahkan pesan stagnasi jika nilai tidak berubah
        if isStagnant:
            print(f"  Tidak ada perubahan nilai maksimum selama {len(previousBestValues)} generasi terakhir.")

        # Jika stagnan terlalu lama, tampilkan pesan lebih jelas
        if generasiStagnant >= 20:
            print(f"  Peringatan: Nilai optimum tidak berubah selama {generasiStagnant} generasi.")
            # currentMutationRate = MUTATION_RATE * 3  <- Tingkatkan mutasi begitu terjadi stagnan
            # print(f" Meningkatkan variasi genetik (mutation rate: {currentMutationRate:.4f})")
            # randomIndCount = POPULATION_SIZE // 20  <- 5% individu baru begitu terjadi stagnan
            # randomIndividuals = InitPopulation(randomIndCount)
            # newPopulation = newPopulation[:-randomIndCount] + randomIndividuals
            # print(f" Menambahkan {randomIndCount} individu acak untuk meningkatkan keragaman populasi.")

        if noImprovementCount >= GENERATIONS_WITHOUT_IMPROVEMENT:
            print(f"\nTidak ada perubahan signifikan selama {GENERATIONS_WITHOUT_IMPROVEMENT} generasi. Evolusi dihentikan.")
            break

        newPopulation = [evaluatedPopulation[i]['chromosome'] for i in range(BEST_INDIVIDUAL)]  # Elitisme

        # Tambahkan variasi dengan meningkatkan mutation rate saat stagnan
        currentMutationRate = MUTATION_RATE
        if generasiStagnant > 25:
            currentMutationRate = MUTATION_RATE * 3  # Tingkatkan mutasi
            MUTATION_RATE = currentMutationRate
            print(f"  Meningkatkan variasi genetik (mutation rate: {currentMutationRate:.4f})")

        while len(newPopulation) < POPULATION_SIZE:
            parents = RoulletteWheelSelection(population, evaluatedPopulation, ROULLETE_SIZE)
            child1, child2 = UniformCrossover(parents[0], parents[1], CROSSOVER_RATE)

            # Mutasi dengan rate yang mungkin sudah dimodifikasi
            if random.random() < currentMutationRate:
                child1 = ScrambleMutation(child1)
            if random.random() < currentMutationRate:
                child2 = ScrambleMutation(child2)

            newPopulation.extend([child1, child2])
            if len(newPopulation) > POPULATION_SIZE:
                newPopulation.pop()

        # Tambahkan beberapa individu acak jika terlalu stagnan
        if generasiStagnant > 35:
            randomIndCount = POPULATION_SIZE // 20  # 5% individu baru
            randomIndividuals = InitPopulation(randomIndCount)
            newPopulation = newPopulation[:-randomIndCount] + randomIndividuals
            POPULATION_SIZE = newPopulation
            print(f"  Menambahkan {randomIndCount} individu acak untuk meningkatkan keragaman populasi.")

        population = newPopulation
    return population, bestSolutionOverall


def printFinalResults(population, startTime, endTime, bestSolutionOverall):
    print("-" * 30)
    print("-------Evolusi Selesai--------")
    print(f"Waktu eksekusi: {endTime - startTime:.2f} detik")

    if bestSolutionOverall:
        print("\nSolusi terbaik ditemukan (Minimalisasi): ")
        print(f"Kromosom: {bestSolutionOverall['chromosome'][:15]}...{bestSolutionOverall['chromosome'][-15:]}")
        print(f"Nilai x1 dan x2: ({bestSolutionOverall['x1']:.4f}, {bestSolutionOverall['x2']:.4f})")
        print(f"Nilai Fungsi Objektif (Minimum): {bestSolutionOverall['objective']:.6f}")
    else:
        print("\nTidak ada solusi terbaik yang ditemukan.")

    # Ringkasan dari loop fitness, crossover, dan mutasi.
    print("\nTop 3 individu di populasi terakhir:")
    finalEvaluatedPopulation = CalculateFitnessValues(population)
    finalEvaluatedPopulation.sort(key=lambda x: x['objective'], reverse=False)
    for i, ind in enumerate(finalEvaluatedPopulation[:3]):
        print(f"  {i + 1}. Nilai fitness: {ind['objective']:.6f}, (x1 = {ind['x1']:.4f}, x2 = {ind['x2']:.4f})")
    print("-" * 30)


def main():
    """
    Fungsi utama yang menjalankan algoritma genetika.
    """
    startTime = time.time()
    population = InitPopulation(POPULATION_SIZE)
    finalPopulation, finalBestSolution = geneticAlgorithm(population)
    endTime = time.time()
    printFinalResults(finalPopulation, startTime, endTime, finalBestSolution) # Hasil akhir


if __name__ == "__main__":
    main()