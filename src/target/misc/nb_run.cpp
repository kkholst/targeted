#include <armadillo>
#include <iostream>
#include <ostream>
#include <target/nb.hpp>
#include <target/utils.hpp>

using namespace arma;
using std::string;
using std::cout;
using std::endl;


int main(int argc, char **argv) {
    mat A(2, 2);
    A.fill(1);
    A(1, 1) = 2;
    cout << target::BLUE << "Naive Bayes test\n\n";
    std::cout << inv(A) << std::endl;
    std::cout << target::COL_RESET;
    return 0;
}

