from IPython.core.interactive import ExecutionResult
import yap
import sys
import syslog
import os

def yap_run_cell(self, s, store_history=True, silent=False,
				 shell_futures=True):

	result = ExecutionResult()

	if (not s) or s.isspace():
		self.last_execution_succeeded = True
		return result

	if store_history:
		result.execution_count = self.execution_count

	def error_before_exec(value):
		result.error_before_exec = value
		self.last_execution_succeeded = False
		return result


	if not self.engine:
		try:
			self.engine = yap.Engine()
		except:
			return error_before_exec( sys.exc_info()[1])

	if not self.q:
		try:
			self.q = self.engine.query(s)
		except SyntaxError:
			return error_before_exec( sys.exc_info()[1])

	cell = s  # cell has to exist so it can be stored/logged

	# Store raw and processed history
	# if not silent:
	#    self.logger.log(cell, s)

	try:
		f = io.StringIO()
		with redirect_stdout(f):
			goal = self.q.next()
		print('{0}'.format(f.getvalue()))
		# Execute the user code
		has_raised = False
		if goal:
			myvs = self.q.namedVarsCopy()
			if myvs:
				i = 0
				for peq in myvs:
					name = peq[0]
					bind = peq[1]
					if bind.isVar():
						var = yap.YAPAtom('$VAR')
						f = yap.YAPFunctor(var, 1)
						bind.unify(yap.YAPApplTerm(f, (name)))
					else:
						i = bind.numberVars(i, True)
						print(name.text() + " = " + bind.text())
			else:
				print("yes")
			if self.q.deterministic():
				self.closeq()
		else:
			print("No (more) answers")
			self.closeq()
	except:
		result.error_in_exec = sys.exc_info()[1]
		# self.showtraceback()
		has_raised = True


	self.last_execution_succeeded = not has_raised
	result.result = self.last_execution_succeeded

	# Reset this so later displayed values do not modify the
	# ExecutionResult
	# self.displayhook.exec_result = None

	#self.events.trigger('post_execute')
	#if not silent:
	#    self.events.trigger('post_run_cell')

	if store_history:
		# Write output to the database. Does nothing unless
		# history output logging is enabled.
		# self.history_manager.store_output(self.execution_count)
		# Each cell is a *single* input, regardless of how many lines it has
		self.execution_count += 1

	return result

def closeq(self):
	if self.q:
		self.q.close()
		self.q = None
