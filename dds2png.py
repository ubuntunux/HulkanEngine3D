import PIL
from PIL import Image
import os
import sys
import glob
import traceback

def dds2png(filepath):
    try:
        new_filepath = os.path.splitext(filepath)[0] + ".png"
        img = Image.open(filepath)
        img.save(new_filepath)
        print(filepath + " -> " + new_filepath)
        os.remove(filepath)
    except:
        print(traceback.format_exc())

if __name__ == '__main__':
    if 1 < len(sys.argv):
        filepath = sys.argv[1]
        if os.path.exists(filepath) and os.path.isdir(filepath):
            for dirpath, dirnames, filenames in os.walk(filepath):
                for filename in filenames:
                    if os.path.splitext(filename)[1].lower() == '.dds':
                        dds2png(os.path.join(dirpath, filename))
