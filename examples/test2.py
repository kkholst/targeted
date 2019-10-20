import target
import target.formula as tg
import pandas as pd
from pkg_resources import resource_filename

d = pd.read_csv(resource_filename('target', '/data/d.csv'), sep=',', header=0)

val = tg.riskreg(d, 'y~a')
print(val['op'])

val = tg.riskreg(d, 'y~a', nuisance='x+z', propensity='x+z')
print(val['op'])

print('Interaction')
val = tg.riskreg(d, 'y~a', interaction='x', nuisance='x+z')
print(val['op'])

val = tg.riskreg(d, 'y~a', interaction='x', nuisance='x+z', propensity='x+z')
print(val['op'])

