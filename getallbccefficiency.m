function DEABCC = getallbccefficiency(x,y)
N = size(x,2);
DEABCC = zeros(N,1);
for k=1:N
    DEABCC(k) = getbcc(x,y,k);
end
end