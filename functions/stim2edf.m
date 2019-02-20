function [] = stim2edf(sentenceString)
%stim2edf Prints sentence to edf
%   Detailed explanation goes here

global Visual;
Pix_per_Letter=14; 

%chars= list(sent) #get characters in sentence
x_start= [];
x_end= [];
y_start= 520;
y_end= 560;

Eyelink('Message', 'DISPLAY TEXT 1');

for i=1:length(sentenceString)% loop to calulate x position of letters
	if i==1
		x_start(i)= Visual.sentPos(1);
		x_end(i)= Visual.sentPos(1)+ Pix_per_Letter;
    else
		x_start(i)= x_end(i-1);
		x_end(i)= x_end(i-1)+ Pix_per_Letter;
    end
    
    Eyelink('Message', ['REGION CHAR ' num2str(i-1) ' 1 ' sentenceString(i) ' ' num2str(x_start(i)) ' ' num2str(y_start) ' ' num2str(x_end(i)) ' ' num2str(y_end)]);
	WaitSecs(0.001); % wait time for consitency purposes with Eyetrack
    Eyelink('Message', 'DELAY 1 MS');
end

end

