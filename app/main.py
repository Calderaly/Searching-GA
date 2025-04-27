import random

# Constants
BITS = 10  # Number of bits for binary representation
DOMAIN_MIN = -10
DOMAIN_MAX = 10

# Function to convert binary representation to real value in the interval [lower, upper]
def binary_to_real(bin_str, lower=DOMAIN_MIN, upper=DOMAIN_MAX):
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

# Function to encode numerical data types into binary
def encode_to_binary(value):
    try:
        if isinstance(value, int):
            return format(value, f'0{BITS}b')
        elif isinstance(value, float):
            return format(int((value - DOMAIN_MIN) / (DOMAIN_MAX - DOMAIN_MIN) * (2**BITS - 1)), f'0{BITS}b')
        else:
            raise ValueError("Unsupported data type for encoding.")
    except ValueError as ve:
        print(ve.args)
        return None

# Objective function to be minimized
def objective(x1, x2):
    return x1**2 + x2**2

# Genetic algorithm parameters
POPULATION_SIZE = 20       # Number of individuals in the population
GENERATIONS = 50           # Number of generations
TOURNAMENT_SIZE = 3        # Number of individuals in tournament selection
CROSSOVER_RATE = 0.8       # Probability of performing crossover
MUTATION_RATE = 0.1        # Probability of mutation on each variable

# Function to generate a random individual
def create_individual():
    return (random.uniform(DOMAIN_MIN, DOMAIN_MAX), random.uniform(DOMAIN_MIN, DOMAIN_MAX))

# Create initial population
def create_population():
    return [create_individual() for _ in range(POPULATION_SIZE)]

# Evaluate fitness of each individual (the smaller the objective value, the better)
def evaluate_population(population):
    return [(individual, objective(*individual)) for individual in population]

# Selection: Tournament Selection
def tournament_selection(evaluated_pop):
    tournament = random.sample(evaluated_pop, TOURNAMENT_SIZE)
    tournament.sort(key=lambda x: x[1])  # Minimize objective
    return tournament[0][0]

# Crossover: Arithmetic Crossover
def crossover(parent1, parent2):
    if random.random() < CROSSOVER_RATE:
        # Encode parents to binary
        parent1_bin = encode_to_binary(parent1[0]) + encode_to_binary(parent1[1])
        parent2_bin = encode_to_binary(parent2[0]) + encode_to_binary(parent2[1])
        
        # Perform crossover on binary strings
        crossover_point = random.randint(1, len(parent1_bin) - 1)
        child1_bin = parent1_bin[:crossover_point] + parent2_bin[crossover_point:]
        child2_bin = parent2_bin[:crossover_point] + parent1_bin[crossover_point:]
        
        # Decode back to real values
        return decode(child1_bin), decode(child2_bin)
    return parent1, parent2

# Mutation: Adding random disturbance to each variable
def mutate(individual):
    x1, x2 = individual
    if random.random() < MUTATION_RATE:
        x1 += random.uniform(-1, 1)
    if random.random() < MUTATION_RATE:
        x2 += random.uniform(-1, 1)
    return max(min(x1, DOMAIN_MAX), DOMAIN_MIN), max(min(x2, DOMAIN_MAX), DOMAIN_MIN)

# Main Genetic Algorithm
def genetic_algorithm():
    population = create_population()
    
    for generation in range(GENERATIONS):
        evaluated_pop = evaluate_population(population)
        new_population = []
        
        while len(new_population) < POPULATION_SIZE:
            parent1 = tournament_selection(evaluated_pop)
            parent2 = tournament_selection(evaluated_pop)
            
            child1, child2 = crossover(parent1, parent2)
            child1 = mutate(child1)
            child2 = mutate(child2)
            
            new_population.append(child1)
            if len(new_population) < POPULATION_SIZE:
                new_population.append(child2)
        
        population = new_population
        
        best_individual, best_value = min(evaluate_population(population), key=lambda x: x[1])
        print(f"Generation {generation + 1}: Best = {best_individual} with value = {best_value:.4f}")
        
        # Print the binary representation of the best individual
        binary_representation_1 = encode_to_binary(best_individual[0])
        binary_representation_2 = encode_to_binary(best_individual[1])
        binary_representation_total = binary_representation_1 + binary_representation_2
        print(f"Binary representation of first best individual {binary_representation_1}, " 
        + f" second best individual {binary_representation_2}, and total best individual: {binary_representation_total}")
        # Print the binary representation of the best value
        best_value_binary = encode_to_binary(best_value)
        print(f"Best value in binary: {best_value_binary}")
    
    evaluated_pop = evaluate_population(population)
    best_individual, best_value = min(evaluated_pop, key=lambda x: x[1])
    return best_individual, best_value

# Running the genetic algorithm
if __name__ == "__main__":
    best_solution, best_score = genetic_algorithm()
    print("\nBest solution found:")
    print(f"x1 = {best_solution[0]:.4f}, x2 = {best_solution[1]:.4f}, with value = {best_score:.4f}")
    
    # Print the binary representation of the best solution
    best_solution_bin_1 = encode_to_binary(best_solution[0])
    best_solution_bin_2 = encode_to_binary(best_solution[1])
    best_solution_bin_total = best_solution_bin_1 + best_solution_bin_2
    print(f"Binary representation of the best first solution {best_solution_bin_1}, the best second solution {best_solution_bin_2}, " 
        + f" and total best solution  {best_solution_bin_total}")
    # Print the binary number of the best score
    best_score_binary = encode_to_binary(int(best_score))
    print(f"Binary representation of the best score: {best_score_binary}")

    # Additional Task: Print comparison of binary numbers after crossover and mutation
    print("\nComparison of binary numbers after crossover and mutation:")
    for individual in create_population():
        binary_representation_1 = encode_to_binary(individual[0])
        binary_representation_2 = encode_to_binary(individual[1])
        binary_representation_total = binary_representation_1 + binary_representation_2
        print(f"Individual: {individual}, Binary1: {binary_representation_1}, Binary2: {binary_representation_2}, " 
        + f" sum: {binary_representation_total}")