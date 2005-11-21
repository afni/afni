function a = angle_dist_nc(p2, p1)

m_cr = cross(p1, p2);
a = atan2(sqrt(sum(m_cr.^2)),dot(p2, p1)); 

return;
