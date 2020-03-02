import target
import target.formula as tg

d = target.get_data()

val = tg.riskreg(d, 'y~a')
print(val['op'])

val = tg.riskreg(d, 'y~a', nuisance='x+z', propensity='x+z')
print(val['op'])

print('Interaction')
val = tg.riskreg(d, 'y~a', interaction='x', nuisance='x+z')
print(val['op'])

val = tg.riskreg(d, 'y~a', interaction='x', nuisance='x+z', propensity='x+z')
print(val['op'])
