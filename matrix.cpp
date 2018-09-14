/*
 * matrix.cpp
 *
 *  Created on: May 14, 2015
 *      Author: Nikhil Patel
 */

#include <iostream>
#include <vector>
#include <windows.h>
#include <math.h>
#include <stdlib.h>

using namespace std;

class Matrix {

	int row;
	int column;
	vector <vector<double> > matrix;

	void set_size(int r, int col) {
		row = r;
		column = col;
	}

	public:
		Matrix(int row, int column){
			//cout << "Matrix Builder \n";
			set_size(row, column);
		}

		void init() {

			double data;
			vector <double> entry;

			for (int counter1 = 0; counter1 < row; counter1++) {
				cout << "Please enter row " << counter1 + 1 << endl;

				for (int counter2 = 0; counter2 < column; counter2++) {
					cout << "Enter Row " << counter1 + 1 << " Column " << counter2 + 1 << ": ";
					cin >> data;
					entry.push_back(data);
				}
				matrix.push_back(entry);
				entry.clear();
			}
		}

		void toString() {
			cout << endl;
			for (int x = 0; x < num_rows(); x++) {
				for (int y = 0; y < num_cols(); y++) {
					cout << matrix[x].at(y) << " ";
				}
				cout << endl;
			}
			cout << endl;
		}

		int num_rows() {
			return row;
		}

		int num_cols() {
			return column;
		}

		double valueAt(int row, int column) {
			return matrix.at(row).at(column);
		}

		double trace(){

			double sum = 0;
			for (int x = 0; x < num_rows(); x++) {
				for (int y = 0; y < num_cols(); y++) {
					if (x == y){
					sum += valueAt(x,y);
					}
				}
			}
			return sum;
		}

		Matrix transpose(){
			vector <double> entry;
			Matrix trans(num_rows(), num_cols());
			for (int x = 0; x < num_cols(); x++) {
				for (int y = 0; y < num_rows(); y++) {
					entry.push_back(valueAt(y,x));
				}
				trans.matrix.push_back(entry);
				entry.clear();
			}
			return trans;
		}

		Matrix inverse() {
			Matrix inv(num_rows(), num_cols());
			if (num_rows() != num_cols()){
				throw "Error: Matrix must be square to have an inverse";
			} else if (determinant() == 0){
				throw "This matrix is not invertible";
			}
			return cofactor().transpose().scalar_multiply(1.0/determinant());
		}

		Matrix add(Matrix addend){

			if (num_rows() != addend.num_rows() or num_cols() != addend.num_cols()){
				throw "Error: Matrices are not the same size";
			}
			Matrix sum(num_rows(), num_cols());
			vector <double> entry;

			for (int x = 0; x < num_rows(); x++) {
				for (int y = 0; y < num_cols(); y++) {
					entry.push_back(valueAt(x,y) + addend.valueAt(x,y));
				}
				sum.matrix.push_back(entry);
				entry.clear();
			}
			return sum;
		}

		Matrix subtract(Matrix subtrahend){

			if (num_rows() != subtrahend.num_rows() or num_cols() != subtrahend.num_cols()){
				throw "Error: Matrices are not the same size";
			}

			Matrix diff(num_rows(), num_cols());
			vector <double> entry;

			for (int x = 0; x < num_rows(); x++) {
				for (int y = 0; y < num_cols(); y++) {
					entry.push_back(valueAt(x,y) - subtrahend.valueAt(x,y));
				}
				diff.matrix.push_back(entry);
				entry.clear();
			}
			return diff;
		}

		Matrix scalar_multiply(double mult) {

			Matrix product(num_rows(), num_cols());
			vector <double> entry;

			for (int x = 0; x < num_rows(); x++) {
				for (int y = 0; y < num_cols(); y++) {
					entry.push_back(valueAt(x,y) * mult);
				}
				product.matrix.push_back(entry);
				entry.clear();
			}
			return product;
		}

		Matrix multiply(Matrix factor){

			if (num_cols() != factor.num_rows()){
				throw "Error: Number of rows in Matrix 1 is not equal to number of columns in Matrix 2";
			}

			double data = 0;
			vector <double> entry;
			Matrix matrix2(num_rows(), factor.num_cols());

			for (int z = 0; z < num_rows(); z++) {
				for (int x = 0; x < num_cols(); x++) {
					for (int y = 0; y < num_cols(); y++) {
						//data += matrix[z].at(y) * factor.matrix[y].at(x);
						data += valueAt(z,y) * factor.valueAt(y,x);
					}
					entry.push_back(data);
					data = 0;
				}
				matrix2.matrix.push_back(entry);
				entry.clear();
			}
				return matrix2;
		}

		Matrix exponent(int exp){

			if (num_rows() != num_cols()){
				throw "Error: Must be a square matrix to multiply by itself";
			}
			Matrix product = *this;
			for (int x = 1; x < exp; x++){
				product = product.multiply(*this);
			}
			return product;
		}


		double determinant() {

			if (num_rows() != num_cols()){
				throw "Error: This matrix must be square";
			}
			vector <double> entry;
			vector <vector <double> > sub;
			double det = 0;
			Matrix submatrix(num_rows() - 1, num_cols() - 1);

			if (num_rows() == 1 and num_cols() == 1) {
				return valueAt(0,0);
			}

			if (num_rows() == num_cols()) {
				if (num_rows() == 2 and num_cols() == 2) {
					det += valueAt(0,0) * valueAt(1,1) - valueAt(0,1) * valueAt(1,0);
				} else {

					for (int val = 0; val < num_cols(); val++){
						for (int x = 1; x < num_rows(); x++) {
							for (int y = 0; y < num_cols(); y++) {
								if (y != val) {
									entry.push_back(valueAt(x,y));
								}
							}
							submatrix.matrix.push_back(entry);
							entry.clear();
						}
						det += pow((-1), val) * valueAt(0,val) * submatrix.determinant();
						submatrix.matrix.clear();
					}
				}
			}
			return det;
		}

		Matrix cofactor() {
			vector <double> entry;
			vector <double> cof;
			Matrix submatrix(num_rows() - 1, num_cols() - 1);
			Matrix cofactor(num_rows(), num_cols());

			for (int x_val = 0; x_val < num_rows(); x_val++){
				for (int y_val = 0; y_val < num_cols(); y_val++){
					for (int x = 0; x < num_rows(); x++) {
						for (int y = 0; y < num_cols(); y++) {
							if (y == y_val or x == x_val) { }else {
								entry.push_back(valueAt(x,y));
							}
						}
						if (x != x_val){
						submatrix.matrix.push_back(entry);
						entry.clear();
						}
					}
					cof.push_back(pow((-1), x_val + y_val) * submatrix.determinant());
					submatrix.matrix.clear();
				}
				cofactor.matrix.push_back(cof);
				cof.clear();
			}
			return cofactor;
		}
};


vector <Matrix> matrices;
vector <string> names;

int get_num(int min, int max){
	int num;
	bool first = true;
	cin.clear();
	while (num < min + 1 or num > max + 1){
		if (first != true) {
			cout << "Invalid number. Please enter an integer between ";
			cout << min + 1 << " and " << max + 1 << " " << endl;
		}
		cin >> num;
		first = false;
	}
	cin.ignore();
	return num - 1;
}

Matrix createMatrix() {

	int row;
	int column;
	string name;
	system("cls");
	cout << "Matrix Builder" << endl;
	cout << "Please enter a name for this matrix: ";
	cin >> name;
	names.push_back(name);

	cout << "Please enter the number of rows: ";
	cin >> row;

	cout << "Please enter the number of columns: ";
	cin >> column;
	cout << endl;
	Matrix m(row, column);
	m.init();
	m.toString();
	cout << "Matrix saved to memory" << endl;
	cin.ignore();
	return m;

}

void show_matrices(){
	for (unsigned int x = 0; x < matrices.size(); x++) {
		cout << "Matrix " << x << " is " << names.at(x) << endl;
	}
}

void command(string input){
	int choice1;
	int choice2;


	if (input == "matrix -n"){
		matrices.push_back(createMatrix());
	}
	else if (input == "matrix -o"){
		cout << "Enter matrix number you wish to view" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		choice1 = get_num(0, matrices.size() - 1);
		matrices.at(choice1).toString();
	}
	else if (input == "matrix -x"){
		cout << "Enter the number of matrix you wish to delete" << endl;
		choice1 = get_num(0, matrices.size() - 1);
		//matrices.erase(matrices.choice1, matrices.choice1);
	}
	else if (input == "matrix -a"){
		cout << "Enter matrix numbers to add" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		cout << "Please select first matrix" << endl;
		choice1 = get_num(0, matrices.size() - 1);
		cout << "Please select second matrix" << endl;
		choice2 = get_num(0, matrices.size() - 1);
		cout << "The sum of the two matrices is:" << endl;
		matrices.at(choice1).add(matrices.at(choice2)).toString();
	}
	else if (input == "matrix -ad"){
		cout << "Enter number of matrix you wish to find adjoint matrix of" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		choice1 = get_num(0, matrices.size() - 1);
		cout << "The adjoint matrix is:" << endl;
		matrices.at(choice1).cofactor().transpose().toString();
	}
	else if (input == "matrix -c"){
		cout << "Enter number of matrix you wish to find cofactor matrix of" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		choice1 = get_num(0, matrices.size() - 1);
		cout << "The cofactor matrix is" << endl;
		matrices.at(choice1).cofactor().toString();
	}
	else if (input == "matrix -d"){
		cout << "Enter number of matrix you wish to find determinant of" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		choice1 = get_num(0, matrices.size() - 1);
		cout << "The determinant of the matrix is: ";
		cout << matrices.at(choice1).determinant() << endl;
	}
	else if (input == "matrix -exp"){
			cout << "Enter number of matrix" << endl;
			cout << "Matrices stored in memory are: " << endl;

			show_matrices();
			choice1 = get_num(-1, matrices.size() - 1);
			cout << "Enter an integer exponent";
			cin >> choice2;
			//choice2 = get_num(0, 1000);
			cout << "The resulting matrix is:" << endl;
			matrices.at(choice1).exponent(choice2).toString();
		}
	else if (input == "matrix -i"){
		cout << "Enter number of matrix you wish to find inverse of" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		choice1 = get_num(0, matrices.size() - 1);
		cout << "The inverse matrix is:" << endl;
		matrices.at(choice1).inverse().toString();
	}
	else if (input == "matrix -k"){
		double factor;
		cout << "Enter matrix number to multiply" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		choice1 = get_num(0, matrices.size() - 1);
		cout << "Enter number to multiply by";
		cin >> factor;
		cout << "The resulting matrix is:" << endl;
		matrices.at(choice1).scalar_multiply(factor).toString();
	}
	else if (input == "matrix -m"){
		cout << "Enter number of the matrices you wish to multiply" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		choice1 = get_num(0, matrices.size() - 1);
		choice2 = get_num(0, matrices.size() - 1);
		cout << "The product matrix is:" << endl;
		matrices.at(choice1).multiply(matrices.at(choice2)).toString();
	}
	else if (input == "matrix -s"){
		cout << "Enter matrix numbers to subtract" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		choice1 = get_num(0, matrices.size() - 1);
		choice2 = get_num(0, matrices.size() - 1);
		cout << "The difference of the two matrices is:" << endl;
		matrices.at(choice1).subtract(matrices.at(choice2)).toString();
	}
	else if (!input.compare("matrix -t")){
		cout << "Enter number of matrix you wish to transpose" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		choice1 = get_num(0, matrices.size() - 1);
		cout << "The transpose of the matrix is:" << endl;
		matrices.at(choice1).transpose().toString();
	}
	else if (input == "matrix -tr"){
		cout << "Enter number of matrix you wish to find trace of" << endl;
		cout << "Matrices stored in memory are: " << endl;

		show_matrices();
		choice1 = get_num(0, matrices.size() - 1);
		cout << "The trace of the matrix is: ";
		cout << matrices.at(choice1).trace() << endl;
	}
	else if (input == "matrix /?"){
		cout << "matrix" << endl;
		cout << '\t' << "-n" << '\t' << "Create a new matrix and store in memory" << endl;
		cout << '\t' << "-o" << '\t' << "View matrix on screen" << endl;
		cout << '\t' << "-x" << '\t' << "Delete a matrix from memory" << endl;
		cout << '\t' << "-a" << '\t' << "Add two stored matrices together" << endl;
		cout << '\t' << "-ad" << '\t' << "Find the adjoint matrix" << endl;
		cout << '\t' << "-c" << '\t' << "Find the cofactor matrix" << endl;
		cout << '\t' << "-d" << '\t' << "Find the determinant of a matrix" << endl;
		cout << '\t' << "-exp" << '\t' << "Put a matrix to an exponent" << endl;
		cout << '\t' << "-i" << '\t' << "Find the inverse of a matrix" << endl;
		cout << '\t' << "-k" << '\t' << "Multiply a matrix by a scalar" << endl;
		cout << '\t' << "-m" << '\t' << "Multiply two stored matrices together" << endl;
		cout << '\t' << "-s" << '\t' << "Subtract two stored matrices from each other" << endl;
		cout << '\t' << "-t" << '\t' << "Find the transpose of a matrix" << endl;
		cout << '\t' << "-tr" << '\t' << "Find the trace of a matrix" << endl;
	}
//	else if (input == "q"){
//		return;
//	}
	else {
		cout << "Invalid command" << endl;
	}
}


int main () {
	string data;

	cout << "First you must create a matrix " << endl;
	while (data != "matrix -n"){
		cout << "Enter matrix -n to create a matrix" << endl;
		getline(cin,data);
		cin.clear();
	}
	matrices.push_back(createMatrix());
	Sleep(1000);
	while (true){
		try {
			cout << "Please enter a command. " << endl;
			cout << "Enter matrix /? for help and q to quit " << endl;
			getline(cin,data);
			command(data);
			if (data == "q") { return 0;}
			cin.clear();
		}catch (const char* e) {
			cout << "Error: " << e << endl;
		}
	}
	return 0;
}
