package Constants;

import java.nio.file.Path;
import java.nio.file.Paths;

public class ProjectConfig {
	
	public final static  String  D_MAP_FILES_PATH ;
	
	static {
		Path currentPathPosition = Paths.get("");
		D_MAP_FILES_PATH = currentPathPosition.toAbsolutePath()+"/src/map/";
	}

}
