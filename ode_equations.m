function dSt=ode_equations(t,St)
%Differential equations 

global B wm wi1 wi2 u um beta b1 phi d1 d2 rr1 rr2 ri2 ri3 al v2 boost wA;  

lamda=(1+b1*cos(2*pi*(t-phi*12)/12))*beta*(St(2*al+1:3*al)+ri2*St(5*al+1:6*al)+ri3*St(8*al+1:9*al))/sum(St);

for i=1:al
    dSt(i,1)=log(1+B(round(t),i))/12*sum(St(:))*(1-v2(round(t),1))  - (u(i)+um(i))*St(i) - wm*St(i); %dM/dt  
    dSt(i+al,1)=wm*St(i) - St(i+al)*sum(lamda(i,:)) - (u(i)+um(i))*St(i+al); %dS0/dt 
    dSt(i+2*al,1)=St(i+al)*sum(lamda(i,:)) - (d1+u(i)+um(i))*St(i+2*al); %dI1/dt
    dSt(i+3*al,1)=d1*St(i+2*al) - (wi1+u(i)+um(i))*St(i+3*al) + v2(round(t),1)*log(1+B(round(t),i))/12*sum(St(:)); %dR1/dt
    dSt(i+4*al,1)=wi1*St(i+3*al) - St(i+4*al)*rr1*sum(lamda(i,:)) - (u(i)+um(i))*St(i+4*al) + wA*St(i+7*al); %dS1/dt 
    dSt(i+5*al,1)=St(i+4*al)*rr1*sum(lamda(i,:)) - (d2+u(i)+um(i))*St(i+5*al); %dI2/dt
    dSt(i+6*al,1)=d2*St(i+5*al) - (wi1+u(i)+um(i))*St(i+6*al); %dR2/dt
    dSt(i+7*al,1)=wi1*St(i+6*al) + wi2*St(i+9*al) - St(i+7*al)*rr2*sum(lamda(i,:)) - (u(i)+um(i)+wA)*St(i+7*al); %dSR/dt
    dSt(i+8*al,1)=St(i+7*al)*rr2*sum(lamda(i,:)) - (d2+u(i)+um(i))*St(i+8*al); %dAI/dt
    dSt(i+9*al,1)=d2*St(i+8*al) - (wi2+u(i)+um(i))*St(i+9*al); %dR/dt
    if i>1 %aging of individuals from one age group to the next and vaccination
        dSt(i,1)=dSt(i,1) + (1-v2(round(t),i))*u(i-1)*St(i-1);
        dSt(i+al,1)=dSt(i+al,1) + (1-v2(round(t),i))*u(i-1)*St(i+al-1);
        dSt(i+2*al,1)=dSt(i+2*al,1) + u(i-1)*St(i+2*al-1);
        dSt(i+3*al,1)=dSt(i+3*al,1) + (1-boost(round(t),i))*u(i-1)*St(i+3*al-1) + v2(round(t),i)*u(i-1)*(St(i-1)+St(i+al-1));
        dSt(i+4*al,1)=dSt(i+4*al,1) + (1-boost(round(t),i))*u(i-1)*St(i+4*al-1); 
        dSt(i+5*al,1)=dSt(i+5*al,1) + u(i-1)*St(i+5*al-1);
        dSt(i+6*al,1)=dSt(i+6*al,1) + u(i-1)*St(i+6*al-1) + boost(round(t),i)*u(i-1)*(St(i+3*al-1)+St(i+4*al-1));
        dSt(i+7*al,1)=dSt(i+7*al,1) + u(i-1)*St(i+7*al-1);
        dSt(i+8*al,1)=dSt(i+8*al,1) + u(i-1)*St(i+8*al-1);
        dSt(i+9*al,1)=dSt(i+9*al,1) + u(i-1)*St(i+9*al-1);
    end
end
