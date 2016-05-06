#include <iostream>
#include <sstream>

int main() {
	std::string ss;
	std::cin >> ss;
	int num;
	
	try {
	std::stringstream(ss) >> num;
}catch(std::exception &e){std::cout << "eeee";}

if (ss.fail()) std::cout << "fiiha";

std::cout << "Number" << num << std::endl;
return 0;
}
	
