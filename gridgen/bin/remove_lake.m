function [mask_mod,mask_map] = remove_lake(mask,lake_tol,igl)

% -------------------------------------------------------------------------------------
%|                                                                                     |
%|                          +----------------------------+                             |
%|                          | GRIDGEN          NOAA/NCEP |                             |
%|                          |      Arun Chawla           |                             |
%|                          |                            |                             |
%|                          | Last Update :  31-Jul-2007 |                             |
%|                          +----------------------------+                             |
%|                                    Arun.Chawla@noaa.gov                             |
%|                          Distributed with WAVEWATCH III                             |
%|                                                                                     |
%|                     Copyright 2009 National Weather Service (NWS),                  |
%|       National Oceanic and Atmospheric Administration.  All rights reserved.        |
%|                                                                                     |
%| DESCRIPTION                                                                         |
%| This routine groups wet cells into independant water bodies with all the wet cells  | 
%| connected to each other sharing the same unique ID                                  |
%|                                                                                     |
%|  [mask_mod,mask_map] = remove_lake(mask,lake_tol,igl)                               | 
%|                                                                                     |
%| INPUT                                                                               |
%|  mask     : Input 2D land/sea mask                                                  |
%|  lake_tol : Tolerance value that determines all the wet cells corresponding to a    | 
%|             particular wet body should be flagged dry or not. If the value is a +ve | 
%|             number then all water bodies having less than this value of total wet   | 
%|             cells will be flagged dry. If the value is 0 then the output and input  |
%|	       masks are unchanged. If the value is -ve then all but the largest water |
%|             body (that with the maximum number of wet cells) is flagged dry         |
%|  igl      : Switch to determine if the grid is global or regional. This is needed   | 
%|             to determine if the first and last cells along a particular row         | 
%|             (longitude/x) are connected or not.                                     |
%|             Options are --                                                          |
%|                igl = 0 for regional (not connected) grids                           |
%|                igl = 1 for global (connected) grids                                 |
%|                                                                                     |
%| OUTPUT                                                                              |
%|  mask_mod : Modified 2D land/sea mask based on the value of lake_tol                |
%|  mask_map : 2D array that has a value of -1 for all land (dry) cells and unique IDs |
%|             for wet cells that are part of a water body. This is used to identify   |
%|             the cells that make up the different water bodies so that they can be   | 
%|             switched between wet and dry by the user if need be.                    | 
%|                                                                                     |
% -------------------------------------------------------------------------------------

[Ny,Nx] = size(mask);

%@@@ Intialize. Start by setting all dry cells to -1 and unmarked wet cells to 0. 
%@@@ Wet cells corresponding to the first water bofy are flagged as 1 and in increasing
%@@@ order therafter

last_mask = 1;
mask_map = mask-1;

%@@@ Determine all the unmarked wet cells

loc = find(mask_map == 0);

%@@@ Intialize while loop if there are unmarked wet cells

if (~isempty(loc))
    new_mask = 1;
else
    new_mask = 0;
end;

%@@@ This loop continues till all the wet cells have been marked

while (new_mask)

     %@@@ Go to the first unmarked wet cell and specify it with a new ID

    [row,col] = find(mask_map == 0,1);
    mask_map(row,col) = last_mask;

    %@@@ Initialize the neighbor flag and put the cell into the neighbor list

    no_near = 0;
    near.x(1) = col;
    near.y(1) = row;

    %@@@ Loop through till no neighbors can be found
    
    while(~no_near)
        
        %@@@ Loop through all elements in neighbor list
           
        N = length(near.x);
        found_mask = 0;
        neighbor_flag = zeros(N,1);

        for i=1:N
            
            %@@@ For each cell determine the neighboring cells
	    %@@@ It is a global grid, account for wrap aorund effect 
            %@@@ in the x direction
 
            this_level = 0;
            if (near.x(i) == 1)
                if (igl == 1)
                    prevx = Nx;
                else
                    prevx = near.x(i);
                end
            else
                prevx = near.x(i)-1;
            end;
            if (near.y(i) == 1)
                prevy = near.y(i);
            else
                prevy = near.y(i)-1;
            end;
            if (near.x(i) == Nx)
                if (igl == 1)
                    nextx = 1;
                else
                    nextx = near.x(i);
                end;
            else
                nextx = near.x(i)+1;
            end;
            if (near.y(i) == Ny)
                nexty = near.y(i);
            else
                nexty = near.y(i)+1;
            end;

            %@@@ Determine if neighboring cells are unmarked wet cells
            %@@@ If yes then mark them with the same ID for the water body
            %@@@ and add this cell to the list of neighboring cells. The
            %@@@ neighbor_flag maintains a record of which cells have all 
            %@@@ marked neighboring wet cells. The found_mask flag mantains
            %@@@ a record of if a new unmarked wet cell was found 

            if (mask_map(near.y(i),prevx) == 0)
                mask_map(near.y(i),prevx) = last_mask;
                near.x(end+1) = prevx;
                near.y(end+1) = near.y(i);
                found_mask = 1;
                neighbor_flag(end+1) = 0;
                this_level = 1;
            end;
            if (mask_map(near.y(i),nextx) == 0)
                mask_map(near.y(i),nextx) = last_mask;
                near.x(end+1) = nextx;
                near.y(end+1) = near.y(i);
                found_mask = 1;
                neighbor_flag(end+1) = 0;
                this_level = 1;
            end;
            if (mask_map(prevy,near.x(i)) == 0)
                mask_map(prevy,near.x(i)) = last_mask;
                near.y(end+1) = prevy;
                near.x(end+1) = near.x(i);
                found_mask = 1;
                neighbor_flag(end+1) = 0;
                this_level = 1;
            end;
            if (mask_map(nexty,near.x(i)) == 0)
                mask_map(nexty,near.x(i)) = last_mask;
                near.y(end+1) = nexty;
                near.x(end+1) = near.x(i);
                found_mask = 1;
                neighbor_flag(end+1) = 0;
                this_level = 1;
            end;

            %@@@ No new unmarked neighboring wet cell was found for this cell
            
            if this_level == 0
                neighbor_flag(i) = 1;
            end;

        end; %@@@ corresponds to for loop over the neighbor list

        %@@@ Check if a new unmarked neighboring wet cell was found

        if (found_mask == 0)

            %@@@ No new neighboring cells found. Set the flag to exit the loop

            no_near = 1;
            loc = find(mask_map == last_mask);
            N1(last_mask) = length(loc);
            fprintf(1,'%d Wet cells set to flag id %d \n',N1(last_mask),last_mask);

        else

            %@@@ One or more new neighboring wet cells were found
            %@@@ Adjust the neighboring cell list to remove cells where all
            %@@@ the neighboring cells are either dry or marked, and repeat
            %@@@ the loop through the neighobring cell list

            x1 = near.x;
            y1 = near.y;
            loc = find(neighbor_flag == 0);
            near.x = [];
            near.y = [];
            near.x = x1(loc);
            near.y = y1(loc);
            clear loc;
            clear x1;
            clear y1;

        end;

    end; %@@@ End of while ~no_near loop. Exit of this loop means all the wet cells
         %@@@ that are connected have been marked with the same ID

    %@@@ Check to see if there are any more unmarked wet cells
    %@@@ If yes then increment the water body ID. If not then 
    %@@@ set the new_mask flag to 0 to exit the while new_mask loop

    clear loc;
    loc = find(mask_map == 0);
    if (~isempty(loc))
        last_mask = last_mask+1;                %@@@ ID for the next water body 
    else
        new_mask = 0;
    end;

end;  %@@@ End of while new_mask loop. Exit of this loop means that all the
      %@@@ wet cells in the grid have been marked           


%@@@ Modify mask based on lake_tol value

mask_mod = mask;

if (lake_tol < 0)                    %@@@ Mask out all the water bodies but the largest one

    [N_max,pos] = max(N1);
    for i = 1:last_mask
        if (i ~= pos)
            loc = find(mask_map == i);
            mask_mod(loc) = 0;
            fprintf(1,'Masking out cells with flag set to %d\n',i);
            clear loc;
        end;
    end;

else                                 %@@@ Mask out all water bodies with # of cells < lake_tol

    for i = 1:last_mask
        if (N1(i) < lake_tol)
            loc = find(mask_map == i);
            mask_mod(loc) = 0;
            fprintf(1,'Masking out cells with flag set to %d\n',i);
            clear loc;
        end;
    end;

end;

return;
