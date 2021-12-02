

















subsu<<lterns<static

>

void
         r -P -l clmatrix_do_something_cb (GObject      *object,
                                      GAsyncResult *result,
                                      gpointer      user_data)
              {
                g_autoptr(GTask) task = user_data;
                g_autoptr(GError) error = NULL;

                g_assert (G_IS_OBJECT (object));
                g_assert (G_IS_ASYNC_RESULT (result));
                g_assert (G_IS_TASK (task));

                g_task_return_boolean (task, TRUE);
              }
              bt

              function_name (void)
              {



               function_name (void)
               {

               }
                y


              }
