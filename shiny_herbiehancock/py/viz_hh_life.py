import csv
with open('mtcars.csv', newline='') as csvfile:
spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')

    for row in spamreader:

        print(', '.join(row))
Spam, Spam, Spam, Spam, Spam, Baked Beans
Spam, Lovely Spam, Wonderful Spam
