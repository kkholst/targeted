
import pkg_resources
import pandas as pd

def get_data(filename='/data/d.csv.gz'):
    """Load example data from the 'targeted' package
    
    """
    inp = pkg_resources.resource_filename('targeted', filename)
    data = pd.read_csv(inp, sep=',', header=0)
    return data
