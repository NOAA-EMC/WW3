function [k,cg,icon,it] = wavenu2(sigma,depth)
%
% Author      : H.L. Tolman
% Last update : 11-Aug-2005
%
%     11-Aug-2005 : Origination.                        ( version 1.00 )
%
%  1. Purpose :
%
%     Compute the wavenumber and the group velocity from the frequency
%     and water depth using the disperion relation.
%
%  2. Method :
%
%     Converted from WAVNU2 in WAVEWATCH III.
%
%  3. Input and output :
%
%       sigma   real   I  Radian frequency                     (Hz)
%       depth   real   I  Water depth                          (m)
%       k       real   O  Wavenumber                          (1/m)
%       cg      real   O  Group velocity                      (m/s)
%       icon    real   O  Convergence indicator.
%                          0: convergence.
%                          1: No convergence.
%       it      real   O  Number of iterations.
%
%  4. Subroutines used :
%
%     None.
%
%  5. Error messages :
%
%  6. Remarks :

%  7. Structure :
%
%  8. Source code :
%
% -------------------------------------------------------------------- %
%  0. Error checking
%
  if nargin ~= 2 
      error ('usage: wavenu2 rad_frequency depth')
  end
%
  [ns,ms] = size(sigma) ;
  [nd,md] = size(depth) ;
  if ( ns ~= 1 ) | ( ms ~= 1 ) | ( nd ~= 1 ) | ( md ~= 1 )
      error ('wavenu2.m: expecting scalar inputs')
  end
  clear ns ms nd md
%
  if depth <= 0
      error ('wavenu2.m: depth must be greater than 0.')
  end      
%
  if sigma == 0
      error ('wavenu2.m: frequency cannot be 0.')
  end      
% -------------------------------------------------------------------- %
%  1. Initializations
%
  grav    = 9.806 ;
  cg      = 0. ;
  k       = 0. ;
  icon    = 1 ;
%
  sig     = abs(sigma) ;
%
% -------------------------------------------------------------------- %
%  2. First guess k
%
  if sig < sqrt(grav/depth)
      k   = sig ./ sqrt(grav*depth) ;
  else
      k   = sig .* sig / grav ;
  end
%
% -------------------------------------------------------------------- %
%  3. Refinement k
%
  nmax    = 25  ;
  eps_it  = 1.e-6 ;
%
  for i = 1:nmax
      k_old    = k ;
      rd       = k_old .* depth ;
      f        = grav .* k_old .* tanh(rd) - sig .^ 2 ;
      if k_old .* depth > 25
          fd  = grav .* tanh(rd) ;
      else
          fd  = grav .* tanh(rd) + grav .* rd / cosh(2.*rd).^2 ;
      end
      k       = k_old - f ./ fd ;
      diff    = abs(k-k_old) ;
      rdiff   = diff ./ k ;
      if ( diff < eps_it ) & ( rdiff < eps_it )
          icon = 0 ;
          break
      end
  end 
  it      = i ;
%
% -------------------------------------------------------------------- %
%  4. Group velocity
%
  rd      = k .* depth ;
  rd      = min ( k.*depth , 25 ) ;
  cg  = 0.5 .* sig ./ k .* ( 1 + 2.*rd / sinh(2.*rd) ) ;
%
% -------------------------------------------------------------------- %
%  5. Correction for negative sigma
%
  if sigma < 0
      k   = -k ;
      cg  = -cg ;
  end
%
% - end of wavenu2 --------------------------------------------------- %
