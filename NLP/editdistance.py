wordA = "intention"
wordB = "execution"

rows, cols = (len(wordA) + 1, len(wordB) + 1)
editMatrix = [[0 for i in range(cols)] for j in range(rows)]

for i in range(0, rows):
    editMatrix[i][0] = i
for j in range(0, cols):
    editMatrix[0][j] = j

cost = 0

for i in range(0, rows - 1):
    for j in range(0, cols - 1):

        insert = editMatrix[i][j+1]
        delete = editMatrix[i+1][j]
        replace = editMatrix[i][j]

        if insert <= delete and insert < replace:
            cost = insert + 1
        elif delete <= insert and delete < replace:
            cost = delete + 1
        else:
            if wordA[i] == wordB[j]:
                cost = replace
            elif delete == replace or insert == replace:
                cost = replace + 1
            else:
                cost = replace + 2
        editMatrix[i+1][j+1] = cost

#Print formatted table with words
for j in range(0, cols - 1):
    if j == 0:
        print("\t\t"),
    print(wordB[j] + "\t"),
print("\n")
for i in range(0, rows):
    for j in range(0, cols):
        if j == 0 and i > 0:
            print(wordA[i-1] + "\t"),
        if i == 0 and j == 0:
            print("\t"),
        print(str(editMatrix[i][j])+ "\t"),
    print("\n")

minEditDistance = editMatrix[rows-1][cols-1]

print("Minimum edit distance: " + str(minEditDistance))
