import numpy as np

def fitness_function(x1, x2):
    return - (np.sin(x1) * np.cos(x2) * np.tan(x1 + x2) + (3/4) * np.exp(1 - np.sqrt(x1**2)))

def initialize_population(size):
    return np.random.uniform(-10, 10, (size, 2))

def decode_chromosome(chromosome):
    return chromosome[0], chromosome[1]

def calculate_fitness(population):
    return np.array([fitness_function(ind[0], ind[1]) for ind in population])

def select_parents(population, fitness):
    probabilities = fitness / fitness.sum()
    return population[np.random.choice(len(population), size=2, p=probabilities)]

def mutate(chromosome, mutation_rate=0.1):
    if np.random.rand() < mutation_rate:
        chromosome[0] += np.random.normal(0, 1)
        chromosome[1] += np.random.normal(0, 1)
    return np.clip(chromosome, -10, 10)

def generative_algorithm(pop_size, generations):
    population = initialize_population(pop_size)
    best_chromosome = None
    best_fitness = float('-inf')

    for _ in range(generations):
        fitness = calculate_fitness(population)
        current_best_idx = np.argmax(fitness)
        
        if fitness[current_best_idx] > best_fitness:
            best_fitness = fitness[current_best_idx]
            best_chromosome = population[current_best_idx]

        new_population = []
        for _ in range(pop_size // 2):
            parents = select_parents(population, fitness)
            offspring1 = mutate(parents[0].copy())
            offspring2 = mutate(parents[1].copy())
            new_population.extend([offspring1, offspring2])
        
        population = np.array(new_population)

    x1, x2 = decode_chromosome(best_chromosome)
    return best_chromosome, x1, x2

# Example usage
best_chromosome, x1, x2 = generative_algorithm(pop_size=100, generations=1000)
print(f"Best Chromosome: {best_chromosome}, x1: {x1}, x2: {x2}")