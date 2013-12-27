#include <stdio.h>
#include <stdlib.h>
#include <math.h>

enum { Size = 5 };

typedef
  char Result;
typedef
  unsigned Index;
typedef
  double Element;
typedef
  Element Vector[Size];
typedef
  Index Permutation[Size];
typedef
  Element Matrix[Size][Size];

const static Element epsilon = 1.0e-6;

static Permutation row;
static Permutation column;

static Element Select_Pivot(Matrix A, Index i)
  { Index R, C, save_R, save_C, hold;
    Element ext;
    ext = A[row[i]][column[i]];
    save_R = i;
    save_C = i;
    for (R = i; R < Size; R += 1)
      for (C = i; C < Size; C += 1)
        if (fabs(A[row[R]][column[C]]) > fabs(ext))
          { ext = A[row[R]][column[C]];
            save_R = R;
            save_C = C; }
    if (save_R != i)
      { hold = row[i];
        row[i] = row[save_R];
        row[save_R] = hold; }
    if (save_C != i)
      { hold = column[i];
        column[i] = column[save_C];
        column[save_C] = hold; }
    return ext; }

Result Gauss(Matrix A, Vector b, Vector x)
  { const Index first = 0, last = Size - 1;
    Index i, j, k;
    Element pivot, coefficient_A, coefficient_b;
    Result res = (Result)0;
		for (i = first; i < Size; i += 1)
			row[i] = column[i] = i;
		for (i = first; i < last; i += 1)
			{ pivot = Select_Pivot(A, i);
				if (fabs(pivot) < epsilon)
					goto exit;
				coefficient_b = b[row[i]] / pivot;
				for (j = i + 1; j < Size; j += 1)
					{ coefficient_A = A[row[i]][column[j]] / pivot;
						for (k = i + 1; k < Size; k += 1)
							A[row[k]][column[j]] -= A[row[k]][column[i]] * coefficient_A;
						A[row[i]][column[j]] = coefficient_A;
						b[row[j]] -= A[row[j]][column[i]] * coefficient_A; }
				b[row[i]] = coefficient_b; }
		if (fabs(A[row[last]][column[last]]) < epsilon)
			goto exit;
		b[row[last]] /= A[row[last]][column[last]];
		for (i = last-1; i > first; i -= 1)
      for (j = last; j > i; j -= 1)
				b[row[j]] -= b[row[i]] * A[row[j]][column[i]];
		for (i = first; i < Size; i += 1)
			x[column[i]] = b[row[i]];
    res = (Result)!0;
  exit:
    return res; }

  static Matrix A = {{  0.0,  0.0,  0.0,  0.0,  1.0 },
                     {  0.0,  0.0,  0.0,  1.0,  0.0 },
                     {  0.0,  0.0,  1.0,  0.0,  0.0 },
                     {  0.0,  1.0,  0.0,  0.0,  0.0 },
                     {  1.0,  0.0,  0.0,  0.0,  0.0 }};
  static Vector b  = {  1.0,  2.0,  3.0,  4.0,  5.0 };
  static Vector x;

int main(int argc, const char * argv[])
{ Index i;
  if (Gauss(A, b, x))
    for (i = 0; i < Size; i += 1)
      printf("x[%d] = %g\n", i, x[i]);
  else
    printf("Failed");
  return 0; }

