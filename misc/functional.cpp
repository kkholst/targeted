#include <functional>
#include <cstdio>
#include <iostream>

using namespace std;

class aClass
{
public:
    void aTest(int a, int b)
    {
        printf("%d + %d = %d", a, b, a + b);
    }
};

template <typename Callable>
void function1(Callable f)
{
    f(1, 1);
}

void test(int a,int b)
{
    printf("%d - %d = %d", a , b , a - b);
}

int main()
{
    aClass obj;

    // Free function
    function1(&test);
    std::cout << std::endl;
    // Bound member function
    using namespace std::placeholders;
    function1(std::bind(&aClass::aTest, obj, _1, _2));
    std::cout << std::endl;
   // Lambda
    function1([&](int a, int b) {
        obj.aTest(a, b);
    });
    std::cout << std::endl;
}
