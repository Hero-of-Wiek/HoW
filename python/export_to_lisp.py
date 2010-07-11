#!BPY
import Blender
import bpy

def export_to_lisp(filepath):
    	out = file(filepath, 'w')
	sce = bpy.data.scenes.active
	ob = sce.objects.active
	mesh = ob.getData(mesh=1)
        out.write("((:vertices")
	for vert in mesh.verts:
            out.write( '#(%f $%f %f)' % (vert.co.x,\
                                          vert.co.y, vert.co.z))
        out.write(")\n")
	out.write('(:faces')
	for face in mesh.faces:
		out.write('(')
		for vert in face.v:
			out.write( '%i ' % (vert.index + 1))
		out.write(')\n')
	out.write("))")
	out.close()
