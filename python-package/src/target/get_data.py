
import pkg_resources
import pandas as pd

def get_data(filename='/data/d.csv'):
    inp = pkg_resources.resource_filename('target', filename)
    data = pd.read_csv(inp, sep=',', header=0)
    return data
