"""Sample methods for testing AST transformation."""

# Notes:
"""
Scope:
    1. Methods
       1. [] global namespace
       2. [] Methods inside class:
          1. [] member method
          2. [] static method
          3. [] classmethod
       3. [] Nested methods
       4. [] Anonymous methods
       5. [] empty methods with only pass statement
       6. [] Methods inherited from parent
    2. Comments
       1. [] Tokenize by regexp
"""


def global_method1():
    print("Non-empty method definition in global namespace!")


class Sample:
    """Collection of different methods for testing."""

    def method1(self):
        """Empty method definition."""
        pass

    @staticmethod
    def static_method1():
        """Empty static method definition."""
        pass

    def method2(self):
        print("Non-empty method definition!")
        print("Non-empty method definition!")
        print("Non-empty method definition!")

    @staticmethod
    def static_method2():
        print("Non-empty method definition!")
        print("Non-empty method definition!")
        print("Non-empty method definition!")

    @classmethod
    def class_method1(cls):
        print("Non-empty class method definition!")
        print("Non-empty class method definition!")
        print("Non-empty class method definition!")

    def nested_method_definitions(self):
        print("Non-empty method definition!")

        def nested_method1_with_args(arg1):
            print("Nested method implementation: arg={0}".format(
                arg1))

        # call nested method
        nested_method1_with_args("something")
