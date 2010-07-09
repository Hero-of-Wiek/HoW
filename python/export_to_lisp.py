def export_to_lisp(filepath):
    	out = file(filepath, 'w')
	sce = bpy.data.scenes.active
	ob = sce.objects.active
	mesh = ob.getData(mesh=1)
        out.write("((:vertices")
	for vert in mesh.verts:
            out.write( '%f %f %f' % (vert.co.x, vert.co.y, vert.co.z))
        out.write(")\n")
        out.close()
