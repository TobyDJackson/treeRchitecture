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
#1. for twig =1
  #2. Calculate weight
  #3. Find parent + calculate horizontal distance between centres.
  #4. Calculate moment of twig on parent. 
  #5. Find parent
  #6. Calculate moment of two cyls on parent = moment + weight*distance
  #7. Stop when you reach the trunk
# For twig ==2
  
# alternatively, for each cyl find all downstream cyls, calculate their CoV and the horizontal distance, calculate the moment.



#tree=my_qsms$treestructs$treestruct[[1]] # This selects one tree
#tree=add_angles(tree) # add angles to this tree
# This function adds the angle each cylinder makes with it's parent
#a=treestruct::get_child_rows(tree,row=100)
#a=treestruct::get_branch(tree,row=790)

