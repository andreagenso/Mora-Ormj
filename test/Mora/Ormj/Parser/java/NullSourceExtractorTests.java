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

package org.springframework.beans.factory.parsing;

import junit.framework.TestCase;

/**
 * @author Rick Evans
 */
public final class NullSourceExtractorTests extends TestCase {

	public void testPassThroughContract() throws Exception {
		Object source  = new Object();
		Object extractedSource = new NullSourceExtractor().extractSource(source, null);
		assertNull("The contract of NullSourceExtractor states that the extraction *always* return null", extractedSource);
	}

	public void testPassThroughContractEvenWithNull() throws Exception {
		Object extractedSource = new NullSourceExtractor().extractSource(null, null);
		assertNull("The contract of NullSourceExtractor states that the extraction *always* return null", extractedSource);
	}

}
