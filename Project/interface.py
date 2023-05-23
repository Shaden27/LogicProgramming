import os
import openai
from pyswip import Prolog

api_key = ""
openai.api_key = api_key
# Initialize a Prolog instance
prolog = Prolog()

# Load the Prolog program
prolog.consult('example.pl')
with open('state.pl', 'w') as f:
    pass
# Query the Prolog program



context = '''sp

*tips*

movieGenre(action / sciFi / drama / romance / comedy / adventure / crime / thriller / animation / mystery / horror / biography / history)
movieRating(low / ok / high) 
movieLength(short / normal / long) 

the prediction should only contain prolog predicates.
'###' is a separator

"Query" only shows up in the question for the value the question is asking for

*start*

There is a movie that is in the horror genre called Scream that is normal length. ### movie(Scream), movieLength(Scream, normal), movieGenre(Scream, horror)

Top Gun Maverick is an action movie that has high ratings and is normal length making it a perfect movie to relax to. ### movie(Top Gun Maverick), movieLength(Top Gun Maverick, normal), movieGenre(Top Gun Maverick, action), movieRating(Top Gun Maverick, high) 

What is the movie rating of Air? ### movie(Air), movieRating(Air, Query)

what is a movie that is directed by Joe Russo? ### movieDirector(Query, 'Joe Russo'),  movie(Query)

what is a movie that is directed by Christopher Nolan? ### movieDirector(Query, 'Christopher Nolan'),  movie(Query)

There is a long movie in the action genre called John Wick and it has a high rating. ### movie(John Wick), movieLength(John Wick, long), movieRating(John Wick, high), movieGenre(John Wick, action)

The movie Guardians of the Galaxy has a rating of 4.5 out of 5 and it is a long action movie. ### movie(Guardians of the Galaxy), movieRating(Guardians of the Galaxy, high), movieLength(Guardians of the Galaxy, long), movieGenre(Guardians of the Galaxy, Action)

What is a long action movie that I can watch? ### movieGenre(Query, action), movieLength(Query, long),  movie(Query)

i want to watch a horror movie. ### movieGenre(Query, horror), movie(Query)
'''

reverseContext = '''
Rules
movieGenre(Query, horror), movie(Query).   How long would you prefer the movie to be?
movieLength(Query, long), movie(Query), movieRating(Query, high). Do you have a preffered movie director?
movieLength(Query, long), movieGenre(Query, action), movie(Query). What rating would you like the movie to have?
movieGenre(Query, action), movie(Query). What rating would you like the movie to have?
movieLength(Query, short), movie(Query). Do you have a preferred genre?
movieRating(Query, ok), movieGenre(Query, action), movie(Query). How long do you want the movie to be?
movieGenre(Query, thriller), movieLength(Query, normal), movie(Query). What rating would you like the movie to have?
'''
# print(prediction['choices'][0]['text'])
# q = prediction['choices'][0]['text'] + '.'
# with open('state.pl', 'w') as f:
#     # Write the code to the file
#     f.write(q)
# results = list(prolog.query(q))
# # Print the results
# for result in results:
#     print(result['Query'])

# #print(prediction['choices'][0]['text'])
prompt = ''
print("Movie Bot : Hey, How can I help you?" + '\n')
while(True):
    userInput = input()
    flag = False
    prompt = prompt + userInput
    #print("Prompt = " + prompt)
    prediction = openai.Completion.create(
        model = "text-davinci-003",
        prompt = context + prompt,
        max_tokens = 100
    )
    q = prediction['choices'][0]['text'] + '.'
    #print(q)
    listQ = q.split("###")
    listQ2 = listQ[1].split('.')
    listQ3 = listQ2[0] + '.'
    #print("LQ1" + listQ[1])
    with open('state.pl', 'w') as f:
        f.write(listQ3)
    results = list(prolog.query(listQ3))
    #print(len(results))
    if len(results) == 1:
        print("Movie Bot : This is the only movie which fits your criteria!")
        print(results[0]['Query'] + '\n')
        print("It was my pleasure to help you")
        break

    with open('state.pl', 'r') as f:
        file_contents = f.read()
    
    prediction = openai.Completion.create(
        model = "text-davinci-003",
        prompt = reverseContext + file_contents,
        max_tokens = 100
    )
    #print("Prediction")
    if prediction['choices'][0]['text'] == '':
        for result in results:
            print("Movie Bot : Is this movie OK?")
            print(result['Query'] + '\n')
            userInput2 = input()
            if userInput2 == 'yes':
                print("Movie Bot : It was my pleasure to help you")
                flag = True
                break
    if flag:
        break
    print("Movie Bot : " + prediction['choices'][0]['text'] + '\n')
    
