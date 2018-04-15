function l = lagrInter( xi,f,x)
    %lagrInter Lagrange interpolation
    %   xi - the known points
    %   f - the value of the function in the xi points
    %   x - the point in which f is approximated
    %   If x appears in xi, the value of f for that point is returned
    I = xi==x;
    if sum(I)==1
       l=f(I);
    else
        m=length(xi)-1;
        n=length(xi);
        a = zeros(1,m+1);
        for i=1:n
           z=xi([1:i-1,i+1:n]);
           p=prod(xi(i)-z);
           a(i)=1/p;
        end
        l=sum((a.*f)./(x-xi))/sum(a./(x-xi));
    end
end

