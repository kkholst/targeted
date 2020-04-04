import subprocess
import os
import logging


def test_cpp():
    logging.info('\n\nTesting C++ code...')
    inp = os.path.join(os.path.dirname(
        os.path.relpath(__file__)), 'bin', 'targeted_c_test')
    if os.path.exists(inp):
        subprocess.check_call(inp)
        logging.info('\nResuming Python tests...\n')
    assert True
