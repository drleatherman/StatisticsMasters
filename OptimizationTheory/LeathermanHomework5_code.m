%%%    Method of Gradient descent
%%%

sdrsdfsdfs = 1;

% evaluate symboled function
function value = eval_func(func, func_symbols, func_values)
  value = subs(func, func_symbols, func_values);
end

%%% compute gradient descent
%%% This requires the inputs are symbols using "symbolic" package (if using Octave)
%%%
%%% func = [x^2, y^2]
%%% syms x y
%%% func_symbols = [x, y]
%%% func_values = [1, 1]
%%% b = 50
%%% 'b' dicates the stepsize in the formula 1/b
function xvect = compute_gradient_desc(xvect, func, func_symbols, func_values, b)
  J = jacobian(func, func_symbols);
  J = subs(J, func_symbols, func_values);
  % since we are dealing in 'syms', it is advised to not use floating point numbres
  % Thus using 'sym(1) / b' lets us use a floating-point number as a symbol
  % vpa(sym) or double(sym) will convert to a real-numbered value.
  xvect = double(xvect - (sym(1) / b) * J * eval_func(func, func_symbols, func_values)');
end


b = 50 % 1 / 0.02
NTrials = 1000;
%NTrials = 5;
epsilon_tolerance = 0.0004;

x_and_y = zeros(NTrials,2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% initialize the values of x and y
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x1 = 1; y1 = 1;

%%% current solution at is stored in vector xvect
xvect = [x1 y1]';

% create symbols for x and y
syms x y
symbols = [x, y];

Gvect = [cos(x) + y - 4,  y*y + x - 9];

for trial = 1:NTrials

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%
    %%% specify the system of non-linear equations we are trying to solve:
    %%% solve for x and y so that f_1(x,y) = 0 and f_2(x,y) = 0,
    %%% where f_1(x,y) = cos(x) + y - 4, and f_2(x,y) = y_square + x - 9
    %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    xvect = compute_gradient_desc(xvect, Gvect, symbols, xvect', b);

    %%% update the current solution by moving in the direction
    %%% of the biggest rate of change

    x_and_y(trial,:) = xvect;

    %x1 = xvect(1);  y1 = xvect(2);

    %%%%
    %%%% if we reach an acceptable level of tolerance,
    %%%% then we can EXIT the For loop and output the solution
    %%%%

    Geval = eval_func(Gvect, symbols, xvect');
    Fvalue = 0.5*(Geval'*Geval);

    if Fvalue < epsilon_tolerance
        break
    end
end


%%%
%%% output the values of x and y
%%%

x1 = xvect(1); y1 = xvect(2);
disp('The approximate solution found by Gradient Descent is ');
disp('x = '); disp( x1); disp('y = '); disp(y1);

disp('Output after '); disp(trial); disp(' iterations  is [x, y] = ');
disp(xvect);

