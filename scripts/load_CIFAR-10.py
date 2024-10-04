import numpy as np

# the file is downloaded from http://www.cs.toronto.edu/~kriz/cifar.html
path = "./datasets/cifar-10-batches-py/test_batch"
# path = "test_batch"

def unpickle(file):
    import pickle
    with open(file, 'rb') as fo:
        dict = pickle.load(fo, encoding='bytes')
    return dict

d = unpickle(path)

import random
random.seed(20241004)

indexes = [i for i in range(10000)]
random.shuffle(indexes)
indexes = indexes[:100]

def save_image(rgb_array, filename):
    from PIL import Image
    import numpy as np
    rgb_array = rgb_array.reshape((3, 32, 32))
    rgb_array = rgb_array.transpose([1, 2, 0])
    # Convert the array to an image and save
    image = Image.fromarray(rgb_array)
    image.save("web_dataset/cifar-10/" + filename)

labels = []
filenames = []

for i in indexes:
    label = d[b'labels'][i+1]
    datum = d[b'data'][i+1]
    filename = d[b'filenames'][i+1]
    filename = filename.decode('utf-8')
    labels.append(label)
    filenames.append(filename)
    save_image(datum, filename)

import pandas as pd
df = pd.DataFrame({
    'label': labels,
    'filename': filenames,
})
df.to_csv('web_dataset/cifar-10/About.csv', index=False)