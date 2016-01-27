% -----------------------------------------------------------------------------------
% -----------------------------------------------------------------------------------
% 
% Author    : Xiao Ling
% Module    : Run on training images
% Date      : 9/26/2015
% Bash      : /Applications/MATLAB_R2015b.app/bin/matlab -nodesktop -nosplash
% 
% -----------------------------------------------------------------------------------
% -----------------------------------------------------------------------------------

%  When the stream is over, output 2^𝑋  − 1
%  Claim: 𝔼[2^𝑋] = 𝑛 + 1
function n = morris(ys)

	x = 0;

	for k = 1:length(ys)
		x = x + flip_(0.5^x);
	end

	n = 2^x - 1


end


function h = flip_(p)
	y = rand(1,1);
	if y<=p
		h = 1;
	else
		h = 0;
	end
end