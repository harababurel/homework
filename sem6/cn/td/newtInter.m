function l = newtInter( xi,fi,x )
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here
    I = xi==x;
    if sum(I)==1
       l=fi(I);
    else
        divtbl=divdif(xi',fi'); %The divided difference table
        divx0=divtbl(1,:); %The first line of the div diff table, corresponding to x0
        l=fi(1);
        for i=2:length(xi)
           z=x-xi(1:i-1);
           l=l+prod(z)*divx0(i+1);
        end
    end
end

