import subprocess
import os
import logging


def test_cpp():
    logging.info('\n\nTesting C++ code...')
    subprocess.check_call(os.path.join(os.path.dirname(
        os.path.relpath(__file__)), 'bin', 'rls_c_test'))
    logging.info('\nResuming Python tests...\n')
    print("aa")
    assert True
