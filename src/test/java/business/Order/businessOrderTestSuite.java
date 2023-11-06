package business.Order;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

@Suite
@SelectClasses({ AdvanceOrderTest.class, AirliftOrderTest.class, BlockadeOrderTest.class, BombOrderTest.class,
		DiplomacyOrderTest.class })
public class businessOrderTestSuite {

}
