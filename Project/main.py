from pyswip import Prolog

# Initialize a Prolog instance
prolog = Prolog()

# Load the Prolog program
prolog.consult('example.pl')

# Query the Prolog program
results = list(prolog.query('movie(X, crime), movieDirector(X, Y), movieLength(X,152)'))

# Print the results
for result in results:
    print(result['X'])