import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

@Suite
@SelectClasses({business.Order.businessOrderTestSuite.class})
public class GlobalTest {

}
