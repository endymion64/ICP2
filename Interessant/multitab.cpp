//============================================================================
// Name        : multitab.cpp
// Author      : Youri Coppens
// Version     :
// Copyright   : fuck off, my code!!
// Description : Multitable, raw implementation for brainstorming
//============================================================================


/* THIS CODE IS BASED UPON THE NDARRAY DOCUMENTATION (PYTHON)
 * http://docs.scipy.org/doc/numpy/reference/arrays.ndarray.html#internal-memory-layout-of-an-ndarray
 */

#include <iostream>

/*
 * First cell determines the dimension.
 * Next #dimension cells determine the size for each dimension
 * Then you need size1*size2*...*sizeN cells to store the data of each cell
 */

/*
 * Example of a 3D-array with dimension sizes 2, 3 and 4.
 * This means there are 24 elements in the multitable
 */

int mtb3d[]={3,
		     2,3,4,
		     1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24};

int* sizs; //placeholder for determining the subarray holding the sizes of the multitable

// IMPORTANT NOTE: each time you call this function, call also delete[] sizs to avoid memory leaks!!!
void sizes(int mtb[]){
	int siz = mtb[0];
	sizs = new int[siz];
	for(int i =0; i<siz; ++i)
		sizs[i]= mtb[i+1];
}

int dimension(int mtb[]){
	return mtb[0];
}

int offsetColMajor(int idx[], int mtb[]){
	int dim = dimension(mtb);
	int offset = 0;
	sizes(mtb);
	for(int k = 0; k < dim; ++k){
		int s = 1;
		for(int j = 0; j < k; ++j){
			s*= sizs[j];
		}
		offset+=(s*idx[k]);
	}
	delete[] sizs;
	return offset;
}

int offsetRowMajor(int idx[], int mtb[]){
	int dim = dimension(mtb);
	int offset = 0;
	sizes(mtb);

	for(int k = 0; k < dim; ++k){
		int s = 1;
		for(int j = k+1; j < dim; ++j){
			s*= sizs[j];
		}
		offset+=(s*idx[k]);
	}
	delete[] sizs;
	return offset;
}

int main() {
	std::cout << "Col-major offset mapping:" << std::endl;
	for(int i=0 ; i<2; ++i){
		for(int j=0 ; j<3; ++j){
			for(int k=0 ; k<4; ++k){
				int m[]= {i,j,k};
				std::cout<< i << ',' << j << ','
						<< k << " -> " << offsetColMajor(m,mtb3d) << std::endl;
			}
		}
	}

	std::cout << "Row-major offset mapping:" << std::endl;
	for(int i=0 ; i<2; ++i){
		for(int j=0 ; j<3; ++j){
			for(int k=0 ; k<4; ++k){
				int m[]= {i,j,k};
				std::cout<< i << ',' << j << ','
						<< k << " -> " << offsetRowMajor(m,mtb3d) << std::endl;
			}
		}
	}
	return 0;
}
