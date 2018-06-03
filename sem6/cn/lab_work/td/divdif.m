function T = divdif(x,f)
    %Input: x - the points xi
    % f - the values of f in in the points xi
    %Output: T - the result matrix
    if length(x) == length(f)
        n = size(x);
        T=zeros(n);
        T(:,1)=f;
        for i = 2:n
           T(1:n-i+1,i) = diff(T(1:n-i+2,i-1))./(x(i:n)-x(1:n-i+1));
        end
        T=[x T];
    else
        T=[];
    end
end
