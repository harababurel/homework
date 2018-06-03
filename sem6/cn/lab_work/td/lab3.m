%1
hold on
clear
disp('Prob 1')
xi=[1930 1940 1950 1960 1970 1980];
f=[123203 131669 150697 179323 203212 226505];
disp('Approximation for 1955')
lagrInter(xi,f,1955)
disp('Approximation for 1995')
lagrInter(xi,f,1995)

%2
clear
disp('Prob 2')
xi=[100,121,144];
f=[10,11,12];
disp('Approximation for sqrt(115)')
lagrInter(xi,f,115)
disp('Actual sqrt(115)')
sqrt(115)

%3
clear
%xi and f used to compute the interpolation
xi=0:0.5:10;
f=(1+cos(pi*xi))./(1+xi);

%xplot and fplot (on an interval with more points) used for plotting
xplot=0:0.01:10;
fplot=(1+cos(pi*xplot))./(1+xplot);
figure
plot(xplot,fplot)
grid on
hold on
%xi2 - xi with a small (0.01) offset
%xi2=0.01:0.5:10.01;
l=zeros(size(xplot));
for i=1:length(l)
    l(i)=lagrInter(xi,f,xplot(i));
end
plot(xplot,l);
legend('f','polynomial');

%Facultative
disp('Facultative')
%1
clear
x=(-pi/4):0.01:(pi/2);
f=cos(x);
xi=[0 pi/4 pi/3];
m=length(xi)-1;
%a
u=zeros(size(x));
for i=1:length(x)
   u(i) = prod(x(i)-xi); 
end
l=zeros(length(x),length(xi));
for k=1:length(x)
    for i=1:m+1
        z=xi([1:i-1,i+1:m+1]);
        l(k,i)=prod((x(k)-z)./(xi(i)-z));
    end
end
figure
pol = polyval(l(1,:),x);
plot(x,pol);
hold on
for i=2:length(l)
    pol = polyval(l(i,:),x);
    plot(x,pol);
end

%b
fi=cos(xi);
disp('Approximation for pi/6 using the barycentric formula')
lagrInter(xi,fi,pi/6)
lpi=[1,3];
for i=1:m+1
   z=xi([1:i-1,i+1:m+1]);
   lpi(i) = prod((pi/6-z)./(xi(i)-z));
end
disp('Approximation for pi/6 using the classical formula')
sum(lpi.*fi)

%c
figure
plot(x,f);
hold on
fpl=zeros(size(x));
for i=1:length(fpl)
   fpl(i)=lagrInter(xi,fi,x(i));
end
plot(x,fpl);
legend('f','polynomial');
%d
%set 1
xi = [-pi/6 0 pi/8];
fi = cos(xi);
figure
plot(x,f);
hold on
fpl=zeros(size(x));
for i=1:length(fpl)
   fpl(i)=lagrInter(xi,fi,x(i));
end
plot(x,fpl);
legend('f','polynomial')

%set 2
xi = linspace(-pi/4,pi/2,3);
fi = cos(xi);
figure
plot(x,f);
hold on
fpl=zeros(size(x));
for i=1:length(fpl)
   fpl(i)=lagrInter(xi,fi,x(i));
end
plot(x,fpl);
legend('f','polynomial')

%2
clear
x=-5:0.01:5;
f=(1+x.^2).^(-1);
figure
plot(x,f);
hold on
grid on
%a
%4th degree => m=4 => 5 nodes are known
xi=linspace(-5,5,5);
fi=(1+xi.^2).^(-1);
lag=zeros(size(x));
for i=1:length(x)
   lag(i)=lagrInter(xi,fi,x(i));
end
plot(x,lag);

%8th degree => m=8 => 9 nodes are known
xi=linspace(-5,5,9);
fi=(1+xi.^2).^(-1);
x2=-4.3:0.01:4.3;
lag=zeros(size(x2));
for i=1:length(x2)
   lag(i)=lagrInter(xi,fi,x2(i));
end
plot(x2,lag);

%14th degree => m=14 => 15 nodes
xi=linspace(-5,5,15);
fi=(1+xi.^2).^(-1);
x3=-4:0.01:4;
lag=zeros(size(x3));
for i=1:length(x3)
   lag(i)=lagrInter(xi,fi,x3(i));
end
plot(x3,lag);
legend('f','4th degree','8th degree','14th degree');


%b
clear
a=-5;b=5;n=15;
%Chebyshev nodes of 1st kind (in interval [-1,1])
cheb=cos(((2*(1:n)-1).*pi)./(2*n));
%Linear transformation of Chebyshev nodes on interval [-5,5]
xi=1/2*((b-a)*cheb+a+b);
fi=(1+xi.^2).^(-1); %value of f in the Chebyshev points
x=linspace(-5,5,1000); %interval used for plotting
f=(1+x.^2).^(-1);
figure;
plot(x,f);
hold on;
l=zeros(size(x));
for i=1:length(l)
    l(i)=lagrInter(xi,fi,x(i));
end
plot(x,l);
legend('f','polynomial');

%c
clear
a=-5;b=5;n=15;
%Chebyshev nodes of 2nd kind (in interval [-1,1])
cheb=cos((pi*(0:n-1))./n);
%Linear transformation of Chebyshev nodes on interval [-5,5]
xi=1/2*((b-a)*cheb+a+b);
fi=(1+xi.^2).^(-1); %value of f in the Chebyshev points
x=linspace(-5,5,1000); %interval used for plotting
f=(1+x.^2).^(-1);
figure;
plot(x,f);
hold on;
l=zeros(size(x));
for i=1:length(l)
    l(i)=lagrInter(xi,fi,x(i));
end
plot(x,l);
legend('f','polynomial');
