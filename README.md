# MyBookReviews
Intended for making my reviews with "deep writing"


The following steps were taken from [Max Deutsch's medium article on deep writing](https://medium.com/deep-writing/how-to-write-with-artificial-intelligence-45747ed073c#.pwxlmp67l).

1. Get book reviews to train and copy/overwrite into DeepWriting > data > tinyshakespeare > input.txt
2. To train the model with new book reviews
```
cd DeepWriting
python train.py
```
Then wait FOREVER I SUPPOSE.
3. To create the sample of Deep Writing, open sample.py and modify the following line of code for the number of words desired for the sample:
```
parser.add_argument(‘-n’, type=int, **default=200**, help=’number of words to sample’)
```
Then run
```
python sample.py
```
