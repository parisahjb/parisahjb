function DEA= getallccrefficiency(x,y)
N = size(x,2);
DEA = zeros(N,1);
for k=1:N
    DEA(k) = getccr(x,y,k);
end
end