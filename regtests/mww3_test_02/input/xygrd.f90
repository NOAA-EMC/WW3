program xygrd

integer :: nx , ny
real    :: sx , sy
real    :: x0 , y0
integer :: i  ,  j

write(*,*)'enter nx,ny '
read(*,*)nx,ny
write(*,*)'nx,ny = ',nx,ny
write(*,*)'enter sx,sy '
read(*,*)sx,sy
write(*,*)'sx,sy = ',sx,sy
write(*,*)'enter x0,y0 '
read(*,*)x0,y0
write(*,*)'x0,y0 = ',x0,y0

open(10,file='xgrd.inp')
open(11,file='ygrd.inp')

do j=1,ny
  do i=1,nx
    write(10,'(1x,e12.6)',advance='no') x0+real(i-1)*sx
    write(11,'(1x,e12.6)',advance='no') y0+real(j-1)*sy
  enddo
  write(10,*)
  write(11,*)
enddo

write(*,*)'x0/1000 = ',(x0/1000.0)
write(*,*)'y0/1000 = ',(y0/1000.0)
write(*,*)'xmax/1000 = ',((x0+real(nx-1)*sx)/1000.0)
write(*,*)'ymax/1000 = ',((y0+real(ny-1)*sy)/1000.0)

write(*,*)' X range (km) : ',(x0/1000.0),'   ',((x0+real(nx-1)*sx)/1000.0)
write(*,*)' Y range (km) : ',(y0/1000.0),'   ',((y0+real(ny-1)*sy)/1000.0)

close(10)
close(11)

end program xygrd
