package org.istanbus.core.runner;

import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Module;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.InitializationError;

import java.lang.annotation.*;

public class GuiceJUnitRunner extends BlockJUnit4ClassRunner {

    private Injector injector;

    @Target(ElementType.TYPE)
    @Retention(RetentionPolicy.RUNTIME)
    @Inherited
    public @interface GuiceModule {
        Class<?> value();
    }

    @Override
    public Object createTest() throws Exception {
        Object obj = super.createTest();
        injector.injectMembers(obj);
        return obj;
    }

    public GuiceJUnitRunner(Class<?> clazz) throws InitializationError {
        super(clazz);
        injector = createInjectorFor(getModulesFor(clazz));
    }

    private Injector createInjectorFor(Class<?> clazz) throws InitializationError {
        Module module = null;
        try {
            module = (Module) clazz.newInstance();
        } catch (InstantiationException e) {
            throw new InitializationError(e);
        } catch (IllegalAccessException e) {
            throw new InitializationError(e);
        }
        return Guice.createInjector(module);
    }

    private Class<?> getModulesFor(Class<?> clazz) throws InitializationError {
        GuiceModule annotation = clazz.getAnnotation(GuiceModule.class);
        if (annotation == null)
            throw new InitializationError(
                    "Missing @GuiceModules annotation for unit test '" + clazz.getName()
                            + "'");
        return annotation.value();
    }
}
