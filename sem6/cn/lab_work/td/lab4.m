%1
hold on
xi=[1 2 3 4 5];
fi=[22 23 25 30 28];
fprintf('f(2.5) aprox = %2.5f\n',newtInter(xi,fi,2.5));
figure
plot(xi,fi,'o');
hold on;
grid on;
xplot=1:0.01:5;
l=zeros(size(xplot));
for i=1:length(l)
   l(i)=newtInter(xi,fi,xplot(i));
end
plot(xplot,l);
legend('f','polynomial');
%2
clear
xi=linspace(0,6,13);
fi=exp(1).^(sin(xi));

xplot=0:0.01:6;
fplot=exp(1).^(sin(xplot));
figure;
plot(xplot,fplot);
hold on;
plot(xi,fi,'o');
l=zeros(size(xplot));
for i=1:length(xplot)
   l(i)=newtInter(xi,fi,xplot(i));
end
plot(xplot,l);
legend('f','nodes','polynomial');

%3
clear
xi=[121 100 144];
fi=[11 10 12];
tbl=zeros(length(xi));
tbl=[fi' tbl];
x=115;
eps=10^(-3);
for i=1:length(tbl)-1
   for j=1:i-1
      tbl(i,j+1) = det([tbl(j,j) xi(j)-x;tbl(i,j) xi(i)-x])/(xi(i)-xi(j));
   end
end

while abs(tbl(length(tbl)-1,length(tbl)-1)-tbl(length(tbl)-2,length(tbl)-2))>=eps
    fi = [fi max(fi)+1];
    xi = [xi fi(end)^2];
    tbl= [tbl zeros(size(tbl,1),1);fi(end) zeros(1,size(tbl,2))];
    i=size(tbl,2)-1;
    for j=1:i-1
      tbl(i,j+1) = det([tbl(j,j) xi(j)-x;tbl(i,j) xi(i)-x])/(xi(i)-xi(j));
    end
    
end
fprintf('|%2.4f - %2.4f| = %1.5f < %1.5f\n',tbl(length(tbl)-1,length(tbl)-1),tbl(length(tbl)-2,length(tbl)-2),abs(tbl(length(tbl)-1,length(tbl)-1)-tbl(length(tbl)-2,length(tbl)-2)),eps);
fprintf('fii=%2.4f\n',tbl(length(tbl)-1,length(tbl)-1));


%facultative
%1
clear
xi=linspace(-5,5,10); %for 20 points - no visible difference
fi=sin(xi);

xplot=-5:0.01:5;
fplot=sin(xplot);
figure
plot(xplot,fplot);
hold on;
grid on;
plot(xi,fi,'*');

l=zeros(size(xplot));
for z=1:length(l)
    tbl=zeros(length(xi));
    tbl=[fi' tbl];
    x=xplot(z);
    eps=10^(-3);
    for i=1:length(tbl)-1
       for j=1:i-1
          tbl(i,j+1) = det([tbl(j,j) xi(j)-x;tbl(i,j) xi(i)-x])/(xi(i)-xi(j));
       end
    end

    while abs(tbl(length(tbl)-1,length(tbl)-1)-tbl(length(tbl)-2,length(tbl)-2))>=eps
        xi = [xi max(xi)+0.1];
        fi = [fi sin(xi(end))];

        tbl= [tbl zeros(size(tbl,1),1);fi(end) zeros(1,size(tbl,2))];
        i=size(tbl,2)-1;
        for j=1:i-1
          tbl(i,j+1) = det([tbl(j,j) xi(j)-x;tbl(i,j) xi(i)-x])/(xi(i)-xi(j));
        end
    end
    l(z)=tbl(length(tbl)-1,length(tbl)-1);
end
plot(xplot,l,'r');
legend('f','nodes','polynomial');


%2
clear
xi=linspace(-1,1,10);
equidist=linspace(-1.1,1.1,12);
equidist=equidist(2:end-1);
m=length(xi)-1;
n=10;
l=zeros(n,length(xi));

for k=1:n
    for i=1:m+1
        z=equidist([1:i-1,i+1:m+1]);
        l(k,i)=prod((xi(k)-z)./(equidist(i)-z));
    end
end
g=zeros(1,length(xi));
for i=1:length(xi)
    g(i)=sum(abs(l(i,:)));
end
figure
plot(xi,g);
hold on;
grid on;

cheb=cos(((2*(1:n)-1).*pi)./(2*n)); %Chebyshev nodes of first kind

for k=1:n
    for i=1:m+1
        z=cheb([1:i-1,i+1:m+1]);
        l(k,i)=prod((xi(k)-z)./(cheb(i)-z));
    end
end
for i=1:length(xi)
    g(i)=sum(abs(l(i,:)));
end
plot(xi,g);


cheb=cos((pi*(0:n-1))./n); %Chebyshev nodes of second kind
for k=1:n
    for i=1:m+1
        z=cheb([1:i-1,i+1:m+1]);
        l(k,i)=prod((xi(k)-z)./(cheb(i)-z));
    end
end
for i=1:length(xi)
    g(i)=sum(abs(l(i,:)));
end
plot(xi,g);

legend('Equidistant','Chebyshev 1st','Chebyshev 2nd');
