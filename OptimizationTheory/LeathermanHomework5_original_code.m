%%%    Method of Gradient descent
%%%

stepsize = 0.00002;

NTrials = 5000;  epsilon_tolerance = 0.0004;

x_and_y = zeros(NTrials,2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% initialize the values of x and y
%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x = 11; y = 11;

%%% current solution at is stored in vector xvect
xvect = [x y]';


for trial = 1:NTrials

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%
    %%% specify the system of non-linear equations we are trying to solve:
    %%% solve for x and y so that f_1(x,y) = 0 and f_2(x,y) = 0,
    %%% where f_1(x,y) = cos(x) + y - 4, and f_2(x,y) = y_square + x - 9
    %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    Gvect = [(x - 10)^4 * (y - 1)^2, (y - 12)^4 * (x - 1)^2]';

    %%% compute the Jacobian matrix

    Jmat = [4 * (x - 10)^3 * (y - 1)^2, (y - 12)^4 * 2 * (x - 1); (x - 10)^4 * 2 * (y - 1), 4 * (y - 12)^3 * (x - 1)^2];

    %%% update the current solution by moving in the direction
    %%% of the biggest rate of change

    xvect = xvect - stepsize*Jmat*Gvect;

    x_and_y(trial,:) = xvect;
    x = xvect(1);  y = xvect(2);

    %%%%
    %%%% if we reach an acceptable level of tolerance,
    %%%% then we can EXIT the For loop and output the solution
    %%%%

    Fvalue = 0.5*(Gvect'*Gvect);

    if Fvalue < epsilon_tolerance
        break
    end
end

%%%
%%% output the values of x and y
%%%

disp('The approximate solution found by Gradient Descent is ');
disp('x = '); disp( x); disp('y = '); disp(y);


disp('Output after '); disp(trial); disp(' iterations  is [x, y] = ');
disp(xvect);
