# This script should give examples of operations on QSMs 
# It relies on QSMs from Pasi Raumonen's TreeQSM
absmax <- function(x) { x[which.max( abs(x) )]}
absmin <- function(x) { x[which.min( abs(x) )]}
radians.to.degrees=function(theta_radians){theta_degrees=theta_radians*57.2957795}


get_angle_3d=function(df_in){
  daughter_vec=as.numeric(df_in[c(1,2,3)])
  parent_vec  =as.numeric(df_in[c(4,5,6)])
  dot_product =daughter_vec%*%parent_vec # These are unit vectors with norm=1
  angle_3d=radians.to.degrees(acos(dot_product))
}

add_angles=function(tree){
 
  # Angle to parent branch in the x-y plane (i.e. from vertical, branching angle from trunk etc)
  tree_temp=tree
  tree_temp$parent_x_cyl =tree[tree$parent_row,]$x_cyl #This gives the z component of the unit vector of the parent cylinder
  tree_temp$parent_y_cyl =tree[tree$parent_row,]$y_cyl #This gives the z component of the unit vector of the parent cylinder
  tree_temp$parent_z_cyl =tree[tree$parent_row,]$z_cyl #This gives the z component of the unit vector of the parent cylinder
  
  tree_temp$larger_z_cyl =tree_temp %>% ungroup() %>% dplyr::select(z_cyl,parent_z_cyl) %>% apply(1,absmax) # find the larger (abs) z_cyl
  tree_temp$smaller_z_cyl=tree_temp %>% ungroup() %>% dplyr::select(z_cyl,parent_z_cyl) %>% apply(1,absmin) # and the smaller
  tree_temp$angle_from_parent_z=radians.to.degrees(acos(tree_temp$smaller_z_cyl/tree_temp$larger_z_cyl)) # calculate the angle between them in the xy plan
  
  # Add 3D angle
  tree_temp$angle_from_parent_3d=tree_temp %>% ungroup() %>% 
    dplyr::select(x_cyl,y_cyl,z_cyl,parent_x_cyl,parent_y_cyl,parent_z_cyl) %>%
    apply(1,get_angle_3d)
  
  #add the variables we want to keep back to tree
  tree$angle_from_parent_z =tree_temp$angle_from_parent_z
  tree$angle_from_parent_3d=tree_temp$angle_from_parent_3d
  return(tree)
  
  # Note that this function adds columns for the angle that each cylinder makes with its parent cylinder
  # To get branching angles you need to select the first cylinder after a bifurcation point using 
  # filter(order_in_branch==1) 
  
}

# Height binning
add_centres_and_height_bin=function(tree,bin_size){
  tree$z_start_from_0=tree$z_start-min(tree$z_start) # reset the z coordinates of the cylinders to start 0
  tree$z_cyl_centre=tree$z_start_from_0+tree$len*tree$z_cyl # find the centre of the cylinders (in z)
  tree$x_cyl_centre=tree$x_start+tree$len*tree$z_cyl # find the centre of the cylinders (in x)
  tree$y_cyl_centre=tree$y_start+tree$len*tree$z_cyl # find the centre of the cylinders (in y)
  tree$height_bin=ceiling((tree$z_cyl_centre)/bin_size) # Add a height bins column
  tree$height_of_bin_m=tree$height_bin*bin_size
  return(tree)
}


#Below here are just my notes

# Branch length



# Moment due to self weight
total_moment_on_this_cyl=function(tree,this_cyl){ # function to calculate the total moment on a cylinder due to all the cylinders outside it
  temp_branch=treestruct::get_branch(tree,row=this_cyl)
  temp_branch$horizontal_distance_to_this_cyl=sqrt(((temp_branch$x_cyl_centre-tree[this_cyl,]$x_cyl_centre)**2)+
                                                     ((temp_branch$y_cyl_centre-tree[this_cyl,]$y_cyl_centre)**2))
  moment_on_this_cyl=sum(temp_branch$horizontal_distance_to_this_cyl*temp_branch$selfweight)
  return(moment_on_this_cyl)
}

selfweight_moment_per_tree=function(tree){
  
  # If density and MOR are not predefined, use values for Quercus robur
  if (length(tree$wood_density_kgm3)==0){
    tree$wood_density_kgm3=865
  }
  if (length(tree$modulus_of_rupture_Pa)==0){
    tree$modulus_of_rupture_Pa=72e6
  }
  tree$selfweight=9.81*tree$vol*tree$wood_density_kgm3 # downwards force due to gravity
  
  tree$moment_selfweight=NA # just to make the column so that I can loop over it
  for (i in c(1:nrow(tree))){ # loop over all the cylinders in a tree and calculate the moment on them due to self weight
    tree$moment_selfweight[i]=total_moment_on_this_cyl(tree,i)
  }
  tree$breaking_moment=pi*tree$modulus_of_rupture_Pa*(tree$rad**3)/32
  tree$risk_factor_selfweight=tree$moment_selfweight/tree$breaking_moment
  # note that this approach ignores reaction wood and assumes the branches are cylindrical
  
  return(tree)
}


# Gravitational safety margins
gravitational_stability=function(tree){
  tree$mass=tree$vol*tree$wood_density_kgm3
  
  # Separate crown from stem using branch_order
  base_cyl=tree[nrow(tree),]
  crown=filter(tree,branch_order>=1)
  stem =filter(tree,branch_order==0)
  
  # Calculate the coordinates of the centres of mass
  tree_x=sum(tree$x_cyl_centre)/sum(tree$mass)
  tree_y=sum(tree$y_cyl_centre)/sum(tree$mass)
  tree_z=sum(tree$z_cyl_centre)/sum(tree$mass)
  crown_x=sum(crown$x_cyl_centre)/sum(crown$mass)
  crown_y=sum(crown$y_cyl_centre)/sum(crown$mass)
  crown_z=sum(crown$z_cyl_centre)/sum(crown$mass)
  stem_x=sum(stem$x_cyl_centre)/sum(stem$mass)
  stem_y=sum(stem$y_cyl_centre)/sum(stem$mass)
  stem_z=sum(stem$z_cyl_centre)/sum(stem$mass)
  
  # Cancluate the offset of the centres of mass
  stability_results=data.frame(tree_offset=sqrt((base_cyl$x_start-tree_x)**2+
                                                (base_cyl$y_start-tree_y)**2),
                               crown_offset=sqrt((base_cyl$x_start-crown_x)**2+
                                                 (base_cyl$y_start-crown_y)**2),
                               stem_offset=sqrt((base_cyl$x_start-stem_x)**2+
                                                (base_cyl$y_start-stem_y)**2))
  
  stability_results$crown_stem_mass_ratio=sum(crown$mass)/sum(stem$mass)
  
  # Calculate Euler max height and safety margin
  C=1.26 # This is the value for a cylindrical column
  stability_results$max_height_Euler=C*((base_cyl$modulus_of_elasticity_Pa/(9.81*base_cyl$wood_density_kgm3))**(1/3))*(base_cyl$dbh**(2/3))
  stability_results$gravitational_safety_margin=base_cyl$treeheight/stability_results$max_height_Euler
  
  return(stability_results)
}



