package model;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

@Suite
@SelectClasses({ CountryTest.class, MapModelTest.class })
public class AllTests {

}
