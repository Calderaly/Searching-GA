# Custom sum function
def custom_sum(*args):
    total = 0
    for number in args:
        total += number
    return total

# Custom Absolute Function
def custom_abs(value):
    # if value < 0:
        # return -value
    # return value
    return -value if value < 0 else value

# Custom max function
def custom_max(*args):
    if not args:
        return None
    max_value = args[0]
    for value in args:
        if value > max_value:
            max_value = value
    return max_value

# Custom min function
def custom_min(*args):
    if not args:
        # raise ValueError("custom_min() arg is an empty sequence")
        return None
    min_value = args[0]
    for arg in args:
        if arg < min_value:
            min_value = arg
    return min_value


def custom_sin(x):
    # Custom sine function using Taylor series expansion
    term = x
    sum_sin = x
    n = 1
    while True:
        term *= -x * x / ((2 * n) * (2 * n + 1))
        sum_sin += term
        if custom_abs(term) < 1e-10:  # Precision threshold
            break
        n += 1
    return sum_sin

def custom_cos(x):
    # Custom cosine function using Taylor series expansion
    term = 1
    sum_cos = 1
    n = 1
    while True:
        term *= -x * x / ((2 * n - 1) * (2 * n))
        sum_cos += term
        if custom_abs(term) < 1e-10:  # Precision threshold
            break
        n += 1
    return sum_cos

def custom_tan(x):
    return custom_sin(x) / custom_cos(x)

def fitness_function(x1, x2):
    return - (custom_sin(x1) * custom_cos(x2) * custom_tan(x1 + x2) + (3/4) * (2.718281828459045 - ((x1**2) ** 0.5)))

def initialize_population(size):
    return [[(random.uniform(-10, 10), random.uniform(-10, 10)) for _ in range(2)] for _ in range(size)]

def decode_chromosome(chromosome):
    return chromosome[0], chromosome[1]

def calculate_fitness(population):
    return [fitness_function(ind[0], ind[1]) for ind in population]

def select_parents(population, fitness):
    total_fitness = custom_sum(fitness)
    probabilities = [f / total_fitness for f in fitness]
    return random.choices(population, weights=probabilities, k=2)

def mutate(chromosome, mutation_rate=0.1):
    if random.random() < mutation_rate:
        chromosome[0] += random.gauss(0, 1)
        chromosome[1] += random.gauss(0, 1)
    return [custom_max(custom_min(gene, 10), -10) for gene in chromosome]

def generative_algorithm(pop_size, generations):
    population = initialize_population(pop_size)
    best_chromosome = None
    best_fitness = float('-inf')

    for _ in range(generations):
        fitness = calculate_fitness(population)
        current_best_idx = fitness.index(custom_max(fitness))
        
        if fitness[current_best_idx] > best_fitness:
            best_fitness = fitness[current_best_idx]
            best_chromosome = population[current_best_idx]

        new_population = []
        for _ in range(pop_size // 2):
            parents = select_parents(population, fitness)
            offspring1 = mutate(parents[0][:])
            offspring2 = mutate(parents[1][:])
            new_population.extend([offspring1, offspring2])
        
        population = new_population

    x1, x2 = decode_chromosome(best_chromosome)
    return best_chromosome, x1, x2

# Example usage
best_chromosome, x1, x2 = generative_algorithm(pop_size=100, generations=1000)
print(f"Best Chromosome: {best_chromosome}, x1: {x1}, x2: {x2}")