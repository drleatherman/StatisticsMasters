%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   Make the Data
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nrow = 60; dimN = 50;  Nsamples = 50; K = 4; 
patch = 20 * ones(28, 20);
background = zeros(nrow, dimN);
seed = 13579;
BigPrime = (2^31) - 1;

Xmat = zeros(nrow * dimN, Nsamples);

index = 1;

for corner1 = 4:8
     for corner2 = 4:8
         for jj = 1:nrow
            for kk = 1:dimN
                seed = mod(16807 * seed, BigPrime);
                background(jj, kk) = 4 * (seed / BigPrime);
            end
         end

         picture = background; 
         picture([(corner1):(corner1 + 27)],[corner2:corner2 + 19]) = patch;

         Xmat(:,index) = reshape(picture,nrow * dimN,1);
         index = index + 1;
     end
end

for corner1 = 26:30
    for corner2 = 26:30
         for jj = 1:nrow
            for kk = 1:dimN
                seed = mod(16807 * seed, BigPrime);
                background(jj,kk) = 4 * (seed/BigPrime);
            end
         end

        picture = background; 
        picture([(corner1):(corner1 + 27)],[corner2:corner2 + 19]) = patch;

        Xmat(:,index) = reshape(picture,nrow * dimN, 1);
        index = index + 1;
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Step 1 of the Algorithm
%%% Find the distance between each pair of data points x_j and x_k
%%% For each data point, find its K nearest neighbours
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% instantiate dist matrix
dist = zeros(dimN, dimN);

% iterate over pairs of to avoid a nested for-loop
for idx_pair = nchoosek(1:size(Xmat)(2), 2)'

  a = idx_pair(1);
  b = idx_pair(2);

  % calculate distance via euclidean dist
  dist_i = norm(Xmat(:, b) - Xmat(:, a));

  % Populate the coords for the matrix
  % This creates a symmetric matrix of distances
  % the diagonal is always 0 since the distance between an element and itself is 0.
  dist(a , b) = dist_i;
  dist(b , a) = dist_i;
end

% sort each vector in asc order
[dist_srt, idx] = sort(dist);

%%%
%%% each column stores the K nearest neighbours for that data point
%%%

% ignore first value because its always 0 since it refers
% to the column value
knn = idx(2:K + 1, :);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Step 2 of the Algorithm
%%% Solve for the weights for each data point
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


weights = [];
for col = 1:dimN
  G = zeros(K, K);
  for coords = [nchoosek(1:K, 2); repmat((1:K)', 1, 2)]'

    r = coords(1);
    c = coords(2);

    g = (Xmat(:, col) - Xmat(:, knn(c, col)))' * (Xmat(:, col) - Xmat(:, knn(r, col)));

    G(r, c) = g;
    G(c, r) = g;
  end

  % Apply regularization to weights to guarantee G is non-singular (and thus invertible)
  weight_i = inv(G + 0.001 * eye(K)) * ones(K, 1);

  % adjust the weights so they sum to one.
  % thus satisifying the constraint that weights must sum to one.
  adj_weight_i = weight_i / sum(weight_i);
  weights = [weights adj_weight_i];
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Step 3 of the Algorithm
%%%  Compute the M matrix, call it M_mat
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

weights(5:dimN, :) = 0
delta = eye(dimN)

M = delta - weights - weights' + sum(weights(1:K, :) .^ 2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% compute the eigenvectors of the matrix M_mat
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[V, D] = eig(M_mat);

 
%%% if the two smallest eigenvalues are both zero,
%%% then use the eigenvectors in the third and fourth column of matrix V.

 

%%% x_axis =   

%%% y_axis = 

 

scatter(x_axis, y_axis)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%   Verfiy that the calculation of matrix M_mat is done correctly.
%%%%   NOTE: This part is Optional.  Only do this if you wish to discuss your project in job interviews.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% write your own code here
