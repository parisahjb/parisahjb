
function [eff, out] = getbcc(x,y,k)


N = size(x,2);
m=size(x,1);
n=size(y,1);

f=zeros(n+m+1,1);
for i=1:n
    f(i)= -y(i,k);
end
f(n+m+1) = -1;
Aeq=zeros(1,n+m+1);
for j=1:m
    Aeq(n+j)= x(j,k);
end
beq=1;
A=[y' -x' ones(N,1)];
b=zeros(N,1);
lb=zeros(m+n+1,1);
lb(n+m+1)= -inf;
[sol, eff] = linprog(f,A,b,Aeq,beq,lb);
eff=-eff;

u = sol(1:n)
v = sol(n+(1:m));
u0 = sol(n+m+1)

out.u = u;
out.v = v;
out.u0 = u0;
out.eff = eff

end