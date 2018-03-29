There are 2 R files which have different codes.

Project.R: 
I used first 56 lines of the code when i was doing data cleaning and manipulation staff. There is a csv file in the folder data which contains the data after all the cleaning and manipulation was done. So, you can start from line 57 and skip codes which doesn't serve your purpose.
Doing variable selection when I used all the variables, I got 8 significant variables. However, there was Multicollinearity and that's why I decided to do variable selection on those 8 variables. I just wanted to avoid using PCA.
After getting 4 significant variables, I used KNN and Naive Bayes methods on the data.
I am a lazy coder, so I use same lines of codes again and again. I know not a very good practice and I am working on it.
For model validation I am using Hold-out method.

Sentiment.R:
Here I am using 6 bi-monthly reports published by RBI to do the sentiment analysis.
I manually copied all the data from the pdf to txt file as there was some error with the encoding which I couldn't decode.
I am using loughran lexicon, which is used to do sentiment analysis of financial articles. 
