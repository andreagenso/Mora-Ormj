/*
 * Copyright 2002-2006 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.beans.propertyeditors;

import junit.framework.TestCase;

/**
 * Unit tests for the {@link StringArrayPropertyEditor} class.
 *
 * @author Rick Evans
 */
public final class StringArrayPropertyEditorTests extends TestCase {

	public void testWithDefaultSeparator() throws Exception {
		StringArrayPropertyEditor editor = new StringArrayPropertyEditor();
		editor.setAsText("0,1,2");
		Object value = editor.getValue();
		assertNotNull(value);
		assertTrue(value instanceof String[]);
		String[] array = (String[]) value;
		for (int i = 0; i < array.length; ++i) {
			assertEquals("" + i, array[i]);
		}
		assertEquals("0,1,2", editor.getAsText());
	}

	public void testWithCustomSeparator() throws Exception {
		StringArrayPropertyEditor editor = new StringArrayPropertyEditor(":");
		editor.setAsText("0:1:2");
		Object value = editor.getValue();
		assertNotNull(value);
		assertTrue(value instanceof String[]);
		String[] array = (String[]) value;
		for (int i = 0; i < array.length; ++i) {
			assertEquals("" + i, array[i]);
		}
		assertEquals("0:1:2", editor.getAsText());
	}

}
