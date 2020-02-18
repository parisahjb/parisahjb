

function [eff, out] = getccr(x,y,k)


N=size(x,2);
m=size(x,1);
n=size(y,1);

f=zeros(n+m,1);
for i=1:n
    f(i)= -y(i,k);
end

Aeq=zeros(1,n+m);
for j=1:m
    Aeq(n+j)=x(j,k);
end
beq=1;
A=[y' -x'];
b=zeros(N,1);
lb=zeros(m+n,1);
[sol, eff] = linprog(f,A,b,Aeq,beq,lb);
eff=-eff;

u = sol(1:n)
v = sol(n+1:end);

out.u = u;
out.v = v;
out.eff = eff

end