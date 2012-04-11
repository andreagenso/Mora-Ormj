package ar.com.slemos.dao;

import java.lang.reflect.ParameterizedType;
import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.criterion.Example;

import ar.com.slemos.exception.UnableToSaveException;
import ar.com.slemos.util.HibernateSessionFactory;

public abstract class GenericDAO<T> {

	@SuppressWarnings("unchecked")
	public Class<T> domainClass = getDomainClass();
	private Session session;

	/**
	 * Method to return the class of the domain object
	 */
	@SuppressWarnings("unchecked")
	protected Class getDomainClass() {
		if (domainClass == null) {
			ParameterizedType thisType = (ParameterizedType) getClass()
					.getGenericSuperclass();
			domainClass = (Class) thisType.getActualTypeArguments()[0];
		}
		return domainClass;
	}

	@SuppressWarnings("unchecked")
	public T load(Long id) {

		T returnValue = (T) getHibernateTemplate().load(domainClass, id);

		session.getTransaction().commit();

		return returnValue;
	}

	public void update(T t) throws UnableToSaveException {
		try {
			getHibernateTemplate().update(t);
			session.getTransaction().commit();
		} catch (HibernateException e) {
			throw new UnableToSaveException(e);
		}
	}

	public void save(T t) throws UnableToSaveException {
		try {
			getHibernateTemplate().save(t);
			session.getTransaction().commit();
		} catch (HibernateException e) {
			throw new UnableToSaveException(e);
		}
	}

	public void delete(T t) {
		getHibernateTemplate().delete(t);
		session.getTransaction().commit();
	}

	@SuppressWarnings("unchecked")
	public List<T> getList() {

		List<T> returnList = getHibernateTemplate().createQuery(
				"from " + domainClass.getName() + " x").list();

		session.getTransaction().commit();

		return returnList;
	}

	public void deleteById(Long id) {
		Object obj = this.load(id);
		getHibernateTemplate().delete(obj);
	}

	public int deleteAll(boolean isSure) {

		int countDeleted = getHibernateTemplate().createQuery(
				"delete " + domainClass.getName()).executeUpdate();

		if (isSure)
			session.getTransaction().commit();
		else
			session.getTransaction().rollback();

		return countDeleted;
	}

	public int count() {
		Integer count = (Integer) getHibernateTemplate().createQuery(
				"select count(*) from " + domainClass.getName() + " x")
				.uniqueResult();

		session.getTransaction().commit();

		return count.intValue();
	}

	@SuppressWarnings("unchecked")
	public List<T> findByExample(T exampleObject) {
		Example example = Example.create(exampleObject).excludeZeroes()
				.enableLike().ignoreCase();

		List<T> list = getHibernateTemplate().createCriteria(domainClass).add(
				example).list();

		session.getTransaction().commit();

		return list;
	}

	private Session getHibernateTemplate() {

		session = HibernateSessionFactory.getSessionFactory()
				.getCurrentSession();

		session.beginTransaction();

		return session;
	}

}
