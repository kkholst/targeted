import netadp.__netadp_c__ as netadpc


def DP(Time, Cost, e_target, hub, OrIdx, DeIdx, WFFE, VFFE, TCrates, Trans,
        out_e, P, E, R, K, W):
    return netadpc.DP(Time, Cost, e_target, hub, OrIdx, DeIdx, WFFE, VFFE,
                       TCrates, Trans, out_e, P, E, R, K, W)