import random, math, time

# Config population
BITS_PER_VARIABEL = 32  # jumlahh bit per variabel
RANGE_UPPER_BOUND = 10.0  # batas bawah x1 dan x2
RANGE_LOWER_BOUND = -10.0  # batas atas x1 dan x2
RANGE_DOMAIN = RANGE_UPPER_BOUND - RANGE_LOWER_BOUND  # range x1 dan x2
CHROMOSOME_LENGTH = BITS_PER_VARIABEL * 2  # panjang chromosome

# Parameter GA
POPULATION_SIZE = 2000  # jumlah populasi
GENERATIONS_SIZE = 200  # jumlah generasi
BEST_INDIVIDUAL = 2  # mempertahankan beberapa individu terbaik
GENERATIONS_WITHOUT_IMPROVEMENT = 40  # limitation untuk hasil yang stagnan

# Operator GA
CROSSOVER_RATE = 0.8  # pc
MUTATION_RATE = 0.01  # pm
ROULLETE_SIZE = 5  # jumlah seleksi pada roullete wheel untuk mendapatkan parent


def InitPopulation(populationSize):
    population = []

    for _ in range(populationSize):
        population.append(''.join(random.choice('01') for _ in range(CHROMOSOME_LENGTH)))

    return population


def ObjectiveFunction(x1, x2):
    try:
        term1 = math.sin(x1) * math.cos(x2) * math.tan(x1 + x2)
        term2 = (3 / 4) * math.exp(1 - math.sqrt(x1 ** 2))
        result = -(term1 + term2)

        if math.isnan(result) or math.isinf(result):
            return float('-inf')
        else:
            return result
    except OverflowError:
        return float('-inf')



def BinaryToFloat(binaryStr):
    decimal = int(binaryStr, 2)

    return RANGE_LOWER_BOUND + (decimal / ((1 << BITS_PER_VARIABEL) - 1)) * RANGE_DOMAIN


def DecodeChromosome(chromosome):
    binX1 = chromosome[:BITS_PER_VARIABEL]
    binx2 = chromosome[BITS_PER_VARIABEL:]

    floatX1 = BinaryToFloat(binX1)
    floatX2 = BinaryToFloat(binx2)

    return floatX1, floatX2



def CalculateFitnessValues(population):  # Ganti populationSize jadi population
    fitness_values = []

    for chromosome in population:
        x1, x2 = DecodeChromosome(chromosome)
        fitness = ObjectiveFunction(x1, x2)

        fitness_values.append(
            {
                'chromosome': chromosome,
                'x1': x1,
                'x2': x2,
                'objective': fitness
            }
        )

    return fitness_values



def RoulletteWheelSelection(population, fitnessValues, size):
    objectiveValues = [item['objective'] for item in fitnessValues]
    maxObjective = max(objectiveValues)
    adjustedFitnessValue = [maxObjective - obj for obj in objectiveValues]

    if all(f == fitnessValues[0] for f in fitnessValues):
        return random.choices(population, k=size)

    totalAdjustedFitness = sum(adjustedFitnessValue)

    winner = [(f / totalAdjustedFitness) for f in adjustedFitnessValue]
    parents = random.choices(population, winner, k=size)

    return parents  # berisi orang tua yang terbaik



def UniformCrossover(parent1, parent2, crossoverRate):
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
    if random.random() < MUTATION_RATE:
        length = CHROMOSOME_LENGTH
        i, j = sorted(random.sample(range(length), 2))

        segment = list(chromosome[i:j + 1])
        random.shuffle(segment)

        return chromosome[:i] + ''.join(segment) + chromosome[j + 1:]

    return chromosome



def main():
    startTime = time.time()
    population = InitPopulation(POPULATION_SIZE)
    bestSolutionOverall = None
    lastBestFitness = float('-inf')
    noImprovementCount = 0

    for generasi in range(GENERATIONS_SIZE):
        evaluatedPopulation = CalculateFitnessValues(population)
        evaluatedPopulation.sort(key=lambda x: x['objective'], reverse=False)  # Sort ascending for minimization

        bestIndividual = evaluatedPopulation[0]  # Get the best individual
        if bestSolutionOverall is None or bestIndividual['objective'] > bestSolutionOverall['objective']:
            bestSolutionOverall = bestIndividual
            lastBestFitness = bestIndividual['objective']
            noImprovementCount = 0
        else:
            noImprovementCount += 1

        print(f"generasi {generasi + 1}/{GENERATIONS_SIZE} = x1: {bestIndividual['x1']:.4f}, x2: {bestIndividual['x2']:.4f}")

        if noImprovementCount >= GENERATIONS_WITHOUT_IMPROVEMENT:
            print(f"\nTidak ada perubahan selama {GENERATIONS_WITHOUT_IMPROVEMENT} generasi. Evolusi dihentikan.")
            break

        newPopulation = [evaluatedPopulation[i]['chromosome'] for i in range(BEST_INDIVIDUAL)]  # Elitism

        while len(newPopulation) < POPULATION_SIZE:
            parents = RoulletteWheelSelection(population, evaluatedPopulation, ROULLETE_SIZE)
            child1, child2 = UniformCrossover(parents[0], parents[1], CROSSOVER_RATE)
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
        print("\nSolusi terbaik ditemukan(Minimalisasi): ")
        print(f"Kromosom: {bestSolutionOverall['chromosome'][:15]}...{bestSolutionOverall['chromosome'][-15:]}")
        print(f"Nilai x1 dan x2: ({bestSolutionOverall['x1']:.4f}, {bestSolutionOverall['x2']:.4f})")
        print(f"Nilai Fungsi Objektif (Minimum): {bestSolutionOverall['objective']:.6f}") # Added the objective function
    else:
        print("\nTidak ada solusi terbaik yang ditemukan.")

    # The summary of the fitness, crossover, and mutation loop. Do not change this
    print("\nTop 3 individu di populasi terakhir:")
    finalEvaluatedPopulation = CalculateFitnessValues(population)
    finalEvaluatedPopulation.sort(key=lambda x: x['objective'], reverse=False)
    for i, ind in enumerate(finalEvaluatedPopulation[:3]):
        print(f" {i + 1}. Nilai fitness: {ind['objective']:.6f}, (x1 = {ind['x1']:.4f}, x2 = {ind['x2']:.4f})")



if __name__ == "__main__":
    main()