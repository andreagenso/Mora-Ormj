package ar.com.slemos.test;

import java.util.List;

import ar.com.slemos.dao.AlumnoDAO;
import ar.com.slemos.dao.UsuarioDAO;
import ar.com.slemos.exception.UnableToSaveException;
import ar.com.slemos.model.Alumno;
import ar.com.slemos.model.Usuario;

public class PruebaDaoGeneric {

	public static void main(String[] args) {

		AlumnoDAO daoTest = new AlumnoDAO();

		List<Alumno> list = daoTest.getList();

		if (list.size() == 0) {
			// Generamos un juego de datos de prueba.mora
			Alumno alumno = new Alumno();
			alumno.setApellido("Perez");
			alumno.setNombre("Pedro");
			alumno.setLegajo(100203);
			alumno.setEdad(20);

			try {
				daoTest.save(alumno);
			} catch (UnableToSaveException e) {
				e.printStackTrace();
			}

			Alumno otroAlumno = new Alumno();
			otroAlumno.setApellido("No me acuerdo");
			otroAlumno.setNombre("Pero si ");
			otroAlumno.setEdad(94);
			otroAlumno.setLegajo(1);
			try {
				daoTest.save(otroAlumno);
			} catch (UnableToSaveException e) {
				e.printStackTrace();
			}

		}

		for (Alumno a : list) {
			System.out.println(a);
		}

		System.out.println("podriamos borrar: " + daoTest.deleteAll(false));

		System.out.println("Buscamos por ejemplo...");

		Alumno ejemplo = new Alumno();
		ejemplo.setLegajo(100203);

		// Usamos la funcionalidad de QBE
		list = daoTest.findByExample(ejemplo);

		System.out.println(list.size());
		if (list.size() == 0) {
			System.err.println("No se encontro nada!");
		}

		for (Alumno a : list) {
			System.out.println(a);
			ejemplo = a;
		}

		System.out
				.println("Vamos a intentar romper alguna regla de negocio...");
		try {
			daoTest.save(ejemplo);
		} catch (UnableToSaveException e) {
			System.err.println("Si, fallo como debia ser");
		}

		System.out.println("Seteamos el usuario inicial...");
		UsuarioDAO usuarioDao = new UsuarioDAO();
		Usuario usuarioAdmin = new Usuario();

		usuarioAdmin.setNombre("admin");
		usuarioAdmin.setPasswd("admin");

		try {
			usuarioDao.save(usuarioAdmin);
		} catch (UnableToSaveException e) {
			System.err.println("Ya se habia creado el usuario admin!");
		}
	}
}
