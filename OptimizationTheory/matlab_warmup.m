%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Step 1
%%
%% Make two column vectors, call them xvect and yvect.
%% Add the two vectors xvect and yvect.
%% Subtract the two vectors xvect and yvect.
%% Take the dot product of two vectors.

xvect = [1 2 3 4 5]';
yvect = [2 4 6 8 10]';

avect = xvect + yvect;

bvect = xvect - yvect;

dot(xvect, yvect)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Step 2
%%
%% Make two matrices A and B.
%% Compute the matrix product, A times B.
%% Compute the matrix proudct, A_transpose times A.
%% Compute the matrix proudct, B times B_transpose.
%% Add two matrices. Subtract two matrices.

Amat = [3 4 5; 6 7 8; 9 9 9];
Bmat = [1 1 1; 2 2 2; 3 3 3];

Cmat = Amat * Bmat;

A2mat = Amat' * Amat;

B2mat = Bmat * Bmat';

Add_mat = Amat + Bmat;

Sub_mat = Amat - Bmat;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Step 3.
%%
%% Make a 3 by 3 matrix, Amat. Make a column vector with 3 entries, v_vect.
%% Let bvect be the matrix multiplication: Amat times v_vect.

Amat = [3 4 5; -1 2 -3; 3 -4 5];
uvect = [2 3 -4]';

bvect = Amat * uvect

% inverse of A
inv(Amat)

%% Given a 3 by 3 matrix A, and given a vector b with 3 entries,
%% here is how to solve for vector x,
%% such that Ax = b.
%% Amat is the matrix we just created, and bvect is the vector we just calculated.
%% We now solve for the vector xvect, where Amat times xvect = bvect.

xvect = inv(Amat) * bvect

%% You should see that xvect has the same entries as uvect.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Step 4.
%%
%% Make a column vector with 10 entries.
%%
%% Count how many entries in the vector are positive.
%% This illustrates two things:
%% how to use a For Loop, and
%% how to use If statement


xvect = [1; -1; 2; -2; 3; 4; 5; 5; -5; 10];

count = 0;
for k = 1:10
  if xvect(k) > 0
    count = count + 1
  end
end

%% To see the value of the variable count, type in the following
count

%% Now, let us compute the Euclidean norm of the vector.
norm(xvect)

%% Let us compute the norm of the vector using a For Loop.
%% This illustrates how to use the For Loop, and the Square Root function

total = 0;
for k = 1:10
  total = total + ( xvect(k) ^2);
end
total = sqrt(total);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Step 5.
%% Make a Gaussian random matrix, Gmat, with 5 rows and 5 columns
%% That means each entry of Gmat is normally distributed with mean 0.
%% Make a matrix, Bmat, with 5 rows and 5 columns, so that
%% row j and column k of Bmat is 1, if row j and column k of Gmat is positive,
%% and row and column k of Bmat is -1, if row j and column k of Gmat < 0.

Gmat = randn(5, 5);
Bmat = zeros(5, 5);

for j = 1:5
  for k = 1:5
    if Gmat(j, k) > 0
      Bmat(j, k) = 1;
    end
    if Gmat(j, k) < 0
      Bmat(j, k) = -1;
    end
  end
end


%% Check that the entries of Bmat are correct
