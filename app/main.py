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

# Custom length function
def custom_len(obj):
    count = 0
    for _ in obj:
        count += 1
    return count

def random_index(max_value):
    return int(max_value * (1.0 * (custom_sum([1 for _ in range(100)]) % 100) / 100))


# Parameter GA
POP_SIZE = 20            # Population size
BITS = 10                # Number of bits per variable (x1 and x2)
TOTAL_BITS = 2 * BITS    # Total bits for one chromosome
PC = 0.7                 # Crossover probability
PM = 0.01                # Mutation probability per bit
GENERATIONS = 100        # Number of iterations/generations

# Function to convert binary representation to real value in the interval [lower, upper]
def binary_to_real(bin_str, lower=-10, upper=10):
    nilai_int = int(bin_str, 2)  # Convert binary to integer
    max_int = 2**BITS - 1         # Maximum possible value
    return lower + (upper - lower) * nilai_int / max_int

# Function to decode chromosome to get values x1 and x2
def decode(chromosome):
    x1_bin = chromosome[:BITS]
    x2_bin = chromosome[BITS:]
    x1 = binary_to_real(x1_bin)
    x2 = binary_to_real(x2_bin)
    return x1, x2

# Objective function to minimize
def objective(x1, x2):
    try:
        return - (sin(x1) * cos(x2) * tan(x1 + x2) + (3/4) * exp(1 - sqrt(x1**2)))
    except Exception:
        return float('inf')

# Fitness evaluation function
def fitness(chromosome):
    x1, x2 = decode(chromosome)
    f_value = objective(x1, x2)
    return 1 / (1 + custom_abs(f_value))

# Initialize population with random chromosomes
def init_population():
    population = []
    for _ in range(POP_SIZE):
        chromosome = ''.join(['0' if (i % 2 == 0) else '1' for i in range(TOTAL_BITS)])
        population.append(chromosome)
    return population

# Simple tournament selection for parents
def selection(pop):
    new_pop = []
    for _ in range(POP_SIZE):
        ind1 = pop[random_index(custom_len(pop))]
        ind2 = pop[random_index(custom_len(pop))]
        new_pop.append(ind1 if fitness(ind1) > fitness(ind2) else ind2)
    return new_pop

# Crossover process between two parents
def crossover(parent1, parent2):
    if random_index(1) < PC:
        point = random_index(TOTAL_BITS - 1) + 1
        return parent1[:point] + parent2[point:], parent2[:point] + parent1[point:]
    return parent1, parent2

# Mutation process: each bit has a chance PM to flip
def mutate(chromosome):
    return ''.join(['1' if bit == '0' and random_index(1) < PM else '0' if bit == '1' and random_index(1) < PM else bit for bit in chromosome])

# Main genetic algorithm function
def genetic_algorithm():
    population = init_population()
    best_chromosome = None
    best_value = float('inf')
    
    for gen in range(GENERATIONS):
        pop_evaluated = [(chrom, objective(*decode(chrom))) for chrom in population]
        for chrom, f_val in pop_evaluated:
            if f_val < best_value:
                best_value = f_val
                best_chromosome = chrom
        
        mating_pool = selection(population)
        new_population = []
        
        for i in range(0, POP_SIZE, 2):
            parent1 = mating_pool[i]
            parent2 = mating_pool[i + 1] if i + 1 < POP_SIZE else mating_pool[0]
            child1, child2 = crossover(parent1, parent2)
            new_population.extend([mutate(child1), mutate(child2)])
        
        population = new_population[:POP_SIZE]
    
    best_x1, best_x2 = decode(best_chromosome)
    return best_chromosome, best_x1, best_x2, best_value

# Execute the program
if __name__ == "__main__":
    best_chrom, best_x1, best_x2, best_obj = genetic_algorithm()
    print("Best Chromosome:", best_chrom)
    print("Value x1 =", best_x1)
    print("Value x2 =", best_x2)
    print("Objective Value =", best_obj)