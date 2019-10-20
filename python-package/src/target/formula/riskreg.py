import target as tg
import numpy as np
import patsy

def riskreg(data, target, model='rr', **kwargs):
    """Risk regression with binary exposure

    :param data: 
    :param target: 
    :param model: 
    :returns: 
    :rtype: 

    """
    interaction = kwargs.get('interaction', '1')
    nuisance    = kwargs.get('nuisance',    '1')
    propensity  = kwargs.get('propensity',  '1')
    weights     = kwargs.get('weights',     '1')

    target = target + '-1'
    y, a = patsy.dmatrices(target, data)
    x1 = patsy.dmatrix(interaction, data)
    x2 = patsy.dmatrix(nuisance, data)
    x3 = patsy.dmatrix(propensity, data)
    w = patsy.dmatrix(weights, data)
    res = tg.riskreg(y, a, x1=x1, x2=x2, x3=x3, weights=w, model=model)
    return res
