# -*- coding: utf-8 -*-

import targeted as tg
import patsy

def riskreg(data: str, target: str, type='rr', **kwargs):
    """Risk regression with binary exposure

    Parameters
    ----------
    data: pandas.DataFrame
    target: str
       Formula describing respone and exposure/treatment of the form 'response ~ exposure'
    type: str
       Type of model (rr: relative risk, rd: risk difference)
    nuisance: str, optional
       nuisance (odds-product) regression formula (string: 'x1+x2+...')
    propensity: str, optional
       propensity model (default: same as nuisance)
    interaction:
       interactions (string: 'x1+x2+...', default: none)

    Returns
    -------
    Riskreg
       Riskreg object
    """

    interaction = kwargs.get('interaction', '1')
    nuisance    = kwargs.get('nuisance', '1')
    propensity  = kwargs.get('propensity', nuisance)
    weights     = kwargs.get('weights', '1')

    target = target + '-1'
    y, a = patsy.dmatrices(target, data)
    x1 = patsy.dmatrix(interaction, data)
    x2 = patsy.dmatrix(nuisance, data)
    x3 = patsy.dmatrix(propensity, data)
    w = patsy.dmatrix(weights, data)
    res = tg.riskreg(y, a, x1=x1, x2=x2, x3=x3, weights=w, type=type)
    return res
